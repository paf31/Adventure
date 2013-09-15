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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Game.Adventure.Script where

import Data.Monoid
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

import Game.Adventure.State
import Game.Adventure.Parser

type MonadGame item m = (MonadState (GameState item) m, MonadWriter [String] m)

data Room item = Room
  { name         :: Location
  , initialItems :: [item]
  , description  :: (MonadGame item m) => m String
  , step         :: (MonadGame item m) => CommandParser (m ())
  }
