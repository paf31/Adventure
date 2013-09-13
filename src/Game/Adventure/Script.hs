-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure.Script
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

{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Game.Adventure.Script where

import Control.Monad.State
import Control.Monad.Writer

import Game.Adventure.Command
import Game.Adventure.State

type MonadGame player item m = (MonadState (GameState player item) m, MonadWriter [String] m)

data Script player item = Script
  { initialize :: (MonadGame player item m) => m ()
  , step       :: (MonadGame player item m) => player -> ScriptCommand item -> m ()
  }
