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

  module Game.Adventure.Command,
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
import Data.Function (fix)

import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W

import Game.Adventure.Command
import Game.Adventure.Parser
import Game.Adventure.State
import Game.Adventure.Actions
import Game.Adventure.Script

untilM :: (Monad m) => m Bool -> m ()
untilM cond = fix $ \action -> cond >>= flip unless action

standardCommands :: (Show item, Eq item, Ord item, MonadGame item m) => Script item -> CommandParser item -> CommandParser (m ())
standardCommands script item = msum
  [ match "look" >> return (look script)
  , moveInDirection <$> (match "move" *> direction)
  , pickUp <$> (match "take" *> item)
  , putDown <$> (match "drop" *> item)
  ]

singlePlayer :: (Eq item, Show item, Ord item) => CommandParser item -> Script item -> IO ()
singlePlayer item script =
  runInputT (setComplete noCompletion defaultSettings)
  $ flip S.evalStateT initialState
  $ do
    greeting <- W.execWriterT $ initialize script
    mapM_ (lift . outputStrLn) greeting
    untilM $ do
      input <- lift $ getInputLine "> "
      (quit, msgs) <- W.runWriterT $ case input of
        Nothing -> return False
        Just line -> do
          location <- currentLocation
          let
            parser = msum
              [ (,) <$> (match "quit" >> return (return ())) <*> pure True
              , (,) <$> standardCommands script item <*> pure False
              , (,) <$> step script location <*> pure False ]
          case evalCommandParser parser line of
            Nothing -> showMessage "Unknown command" >> return False
            Just (action, quit) -> action >> return quit
      mapM_ (lift . outputStrLn) msgs
      return quit

