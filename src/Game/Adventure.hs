-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure
-- Copyright   :  (c) Phil Freeman, Jeff Polakow 2013
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Adventure (

  module Game.Adventure.Parser,
  module Game.Adventure.Actions,

  standardCommands,

  standardRoom,

  play,

  setRoomDescription,
  addToRoom,
  removeFromRoom,
  itemsInRoom

) where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import System.Console.Haskeline
import Control.Monad.State.Class
import Control.Monad.Writer.Class

import Data.Maybe (fromMaybe)
import Data.Function (fix)
import Data.List (find, delete, intercalate)

import qualified Data.Map as M

import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W

import Game.Adventure.Parser
import Game.Adventure.Actions

untilM :: (Monad m) => m Bool -> m ()
untilM cond = fix $ \action -> cond >>= flip unless action


-- R Monad

newtype R item m a = R { unR :: S.StateT ([item], String) m a } deriving (Functor, Monad)

instance (S.MonadState (GameState item) m) => (S.MonadState (GameState item) (R item m)) where
  get = R $ lift S.get
  put gs = R $ lift $ S.put gs

deriving instance (W.MonadWriter (Reset [String]) m) => (W.MonadWriter (Reset [String]) (R item m))

runR :: R item m a -> [item] -> String -> m (a, ([item], String))
runR r is s = S.runStateT (unR r) (is, s)

setRoomDescription :: (Monad m) => String -> R item m ()
setRoomDescription s = R $ S.modify (\(is, _) -> (is, s))

addToRoom :: (Monad m) => item -> R item m ()
addToRoom i = R $ S.modify (\(is, d) -> (i:is, d))

removeFromRoom :: (Monad m, Eq item) => item -> R item m ()
removeFromRoom i = R $ S.modify (\(is, d) -> (delete i is, d))

itemsInRoom :: (Functor m, Monad m) => R item m [item]
itemsInRoom = R $ fmap fst S.get

-- Rooms

standardRoom :: (Show item, Eq item) => Location -> String -> CommandParser item -> (forall m. (MonadGame item m) => s -> CommandParser (R item m s)) -> [item] -> s -> Room item
standardRoom loc desc item run items s = roomState loc (mkDesc items) (\(items, s) ->
    fmap (\r -> do { (s', (items', desc')) <- runR r items desc; return ((items', s'), desc') })
      (standardRoomCommands item items s <|> (run s)))
      (items, s)
  where
    standardRoomCommands :: (MonadGame item m, Show item, Eq item) => CommandParser item -> [item] -> s -> CommandParser (R item m s)
    standardRoomCommands item items s = msum
      [ pickUp s <$> (match "take" *> item)
      , putDown s <$> (match "drop" *> item)
      ]
    pickUp :: (MonadGame item m, Show item, Eq item) => s -> item -> R item m s
    pickUp s item = removeFromRoom item >> addToInventory item >> updateDescription >> return s
    putDown :: (MonadGame item m, Show item, Eq item) => s -> item -> R item m s
    putDown s item = addToRoom item >> removeFromInventory item >> updateDescription >> return s
    updateDescription :: (Functor m, Monad m, Show item) => R item m ()
    updateDescription = do
       items <- itemsInRoom
       setRoomDescription $ mkDesc items
    mkDesc :: (Show item) => [item] -> String
    mkDesc items = unlines $ desc : map (("You can see " ++) . (++ ".") . show) items

-- Single Player Mode

newtype Game item a = Game { runGame :: W.WriterT (Reset [String]) (S.StateT (AllState item) (InputT IO)) a } deriving (Functor, Monad, S.MonadState (AllState item), W.MonadWriter (Reset [String]))

newtype Global item a = Global { runGlobal :: Game item a } deriving (Functor, Monad, W.MonadWriter (Reset [String]))

instance S.MonadState (GameState item) (Global item) where
  get = Global $ fmap globalState S.get
  put gs = Global $ S.get >>= \as -> S.put $ as { globalState = gs }

standardCommands :: (Show item, Eq item, Ord item) => Room item -> CommandParser item -> CommandParser (Global item ())
standardCommands room item = msum
  [ match "look" >> return (look room)
  ]

step :: Room item -> CommandParser (Global item ())
step room = fmap step' (runRoom room) where
  step' act = Global $ do
    room' <- runGlobal act
    S.modify $ \gs -> gs { rooms = M.insert (name room) room' (rooms gs) }

liftInput :: InputT IO a => Global item a
liftInput = Global . Game . lift . lift

play :: (Eq item, Show item, Ord item) => CommandParser item -> AllState item -> IO ()
play item st =
  runInputT (setComplete noCompletion defaultSettings)
  $ flip S.evalStateT st
  $ fmap fst $ W.runWriterT
  $ runGame
  $ runGlobal
  $ do
    untilM $ do
      input <- liftInput $ getInputLine "> "
      (quit, Reset msgs) <- listen $ case input of
        Nothing -> return False
        Just line -> do
          as <- Global S.get
          location <- currentLocation
          let
            room = fromMaybe (error "an unknown location") $ M.lookup location (rooms as)
            parser = msum
              [ (,) <$> (match "quit" >> return (return ())) <*> pure True
              , (,) <$> standardCommands room item <*> pure False
              , (,) <$> step room <*> pure False ]
          case evalCommandParser parser line of
            Nothing -> showMessage "Unknown command" >> return False
            Just (action, quit) -> action >> return quit
      mapM_ (liftInput . outputStrLn) (fromMaybe [] msgs)
      clearLog
      return quit

