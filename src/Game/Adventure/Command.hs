-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure.Command
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

module Game.Adventure.Command where

data Direction = North | South | East | West deriving (Show, Eq)

newtype Command item = Command { runCommand :: Either (SystemCommand item) (ScriptCommand item) } deriving (Show, Eq)

data SystemCommand item = Take item | Move Direction | Look | Quit deriving (Show, Eq)

data ScriptCommand item
  = Combine item item
  | Use item deriving (Show, Eq)
