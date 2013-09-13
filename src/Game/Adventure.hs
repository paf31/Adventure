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

  runScript,

  singlePlayer

) where

import Control.Monad
import Control.Monad.Trans
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

runScript :: (Eq item, Show item, Ord item, Ord player, MonadGame player item m) => Command item -> player -> Script player item -> m Bool
runScript (Command (Right cmd)) player script = do
  step script player cmd
  return False
runScript (Command (Left Look)) player script = do
  st <- S.get
  location <- currentLocation player
  inventory <- inventory player
  itemsVisible <- itemsAtCurrentLocation player
  showMessage $ "You are at " ++ show location ++ ". "
  flip mapM_ inventory $ \item -> showMessage $ "You have '" ++ show item ++ "'."
  flip mapM_ itemsVisible $ \item -> showMessage $ "You can see '" ++ show item ++ "'."
  return False
runScript (Command (Left (Move direction))) player _ = do
  moveInDirection player direction
  return False
runScript (Command (Left (Take item))) player _ = do
  pickUp player item
  return False
runScript (Command (Left Quit)) _ _ = return True

singlePlayer :: (Eq item, Show item, Ord item, Ord player) => CommandParser item -> player -> Script player item -> IO ()
singlePlayer item player script =
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
          case readCommand (command item) line of
            Nothing -> do
              showMessage "Unknown command"
              return False
            Just command -> runScript command player script
      mapM_ (lift . outputStrLn) msgs
      return quit

