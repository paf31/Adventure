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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Game.Adventure (

  module Game.Adventure.Parser,
  module Game.Adventure.State,
  module Game.Adventure.Actions,
  module Game.Adventure.Script,

  standardCommands,

  singlePlayer

) where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import System.Console.Haskeline
import Control.Monad.State.Class
import Control.Monad.Writer.Class

import Data.Maybe (fromMaybe)
import Data.Function (fix)
import Data.List (find)

import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W

import Game.Adventure.Parser
import Game.Adventure.State
import Game.Adventure.Actions
import Game.Adventure.Script

untilM :: (Monad m) => m Bool -> m ()
untilM cond = fix $ \action -> cond >>= flip unless action

standardCommands :: (Show item, Eq item, Ord item, MonadGame item m) => Room item -> CommandParser item -> CommandParser (m ())
standardCommands room item = msum
  [ match "look" >> return (look room)
  , pickUp <$> (match "take" *> item)
  , putDown <$> (match "drop" *> item)
  ]

setupRoom :: (Ord item, MonadGame item m) => Room item -> m ()
setupRoom room = mapM_ (addItemAt $ name room) . initialItems $ room

singlePlayer :: (Eq item, Show item, Ord item) => CommandParser item -> [Room item] -> Room item -> IO ()
singlePlayer item rooms startAt =
  runInputT (setComplete noCompletion defaultSettings)
  $ flip S.evalStateT (initialState (name startAt))
  $ do
    W.runWriterT $ mapM_ setupRoom rooms
    untilM $ do
      input <- lift $ getInputLine "> "
      (quit, msgs) <- W.runWriterT $ case input of
        Nothing -> return False
        Just line -> do
          location <- currentLocation
          let
            room = fromMaybe (error "an unknown location") $ find ((==) location . name) rooms
            parser = msum
              [ (,) <$> (match "quit" >> return (return ())) <*> pure True
              , (,) <$> standardCommands room item <*> pure False
              , (,) <$> step room <*> pure False ]
          case evalCommandParser parser line of
            Nothing -> showMessage "Unknown command" >> return False
            Just (action, quit) -> action >> return quit
      mapM_ (lift . outputStrLn) msgs
      return quit

