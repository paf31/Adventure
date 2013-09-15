-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure.SinglePlayer
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

module Game.Adventure.SinglePlayer (

  standardCommands,

  play

) where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import System.Console.Haskeline
import Control.Monad.State.Class
import Control.Monad.Writer.Class

import Data.Maybe (fromMaybe)
import Data.Function (fix)
import Data.List (find, delete)

import qualified Data.Map as M

import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W

import Game.Adventure.Parser
import Game.Adventure.Actions

-- Helper Functions

untilM :: (Monad m) => m Bool -> m ()
untilM cond = fix $ \action -> cond >>= flip unless action

-- Game Monads

newtype Game item a = Game { runGame :: W.WriterT (Reset [String]) (S.StateT (AllState item) (InputT IO)) a } deriving (Functor, Monad, S.MonadState (AllState item), W.MonadWriter (Reset [String]))

newtype Global item a = Global { runGlobal :: Game item a } deriving (Functor, Monad, W.MonadWriter (Reset [String]))

instance S.MonadState (GameState item) (Global item) where
  get = Global $ fmap globalState S.get
  put gs = Global $ S.get >>= \as -> S.put $ as { globalState = gs }

step :: Room item -> CommandParser (Global item ())
step room = fmap step' (runRoom room) where
  step' act = Global $ do
    room' <- runGlobal act
    S.modify $ \gs -> gs { rooms = M.insert (name room) room' (rooms gs) }

liftInput :: InputT IO a => Global item a
liftInput = Global . Game . lift . lift

-- Single Player Game

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
              , (,) <$> standardCommands room <*> pure False
              , (,) <$> step room <*> pure False ]
          case evalCommandParser parser line of
            Nothing -> showMessage "Unknown command" >> return False
            Just (action, quit) -> action >> return quit
      mapM_ (liftInput . outputStrLn) (fromMaybe [] msgs)
      clearLog
      return quit

