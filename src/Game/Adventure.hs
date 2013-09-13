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
{-# LANGUAGE FlexibleContexts #-}

module Game.Adventure (

  module Game.Adventure.Command,
  module Game.Adventure.Parser,
  module Game.Adventure.State,
  module Game.Adventure.Actions,

  Script,

  singlePlayer

) where

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline
import Control.Monad.State.Class
import Data.Function (fix)

import qualified Control.Monad.State as S
import qualified Control.Monad.Cont as C

import Game.Adventure.Command
import Game.Adventure.Parser
import Game.Adventure.State
import Game.Adventure.Actions

type Script player item = forall m. (MonadText m, MonadState (GameState player item) m) => m (player -> ScriptCommand item -> m ())

instance (Monad m, MonadIO m) => MonadText (InputT m) where
  showMessage = outputStrLn

untilM :: (Monad m) => m Bool -> m ()
untilM cond = fix $ \action -> cond >>= flip unless action

singlePlayer :: (Eq item, Show item, Ord item, Ord player) => CommandParser item -> player -> Script player item -> IO ()
singlePlayer item player script =
  runInputT (setComplete noCompletion defaultSettings)
  $ flip S.evalStateT initialState $ do
    handler <- script
    untilM $ do
      input <- lift $ getInputLine "> "
      case input of
        Nothing -> return False
        Just line -> do
          let cmd = readCommand (command item) line
          case fmap runCommand cmd of
            Nothing -> do
              showMessage "Unknown command"
              return False
            Just (Right cmd) -> do
              handler player cmd
              return False
            Just (Left Look) -> do
              st <- S.get
              location <- currentLocation player
              inventory <- inventory player
              itemsVisible <- itemsAtCurrentLocation player
              showMessage $ "You are at " ++ show location ++ ". "
              flip mapM_ inventory $ \item -> showMessage $ "You have '" ++ show item ++ "'."
              flip mapM_ itemsVisible $ \item -> showMessage $ "You can see '" ++ show item ++ "'."
              return False
            Just (Left (Move direction)) -> do
              moveInDirection player direction
              return False
            Just (Left (Take item)) -> do
              pickUp player item
              return False
            Just (Left Quit) -> return True
