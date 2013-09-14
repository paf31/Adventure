-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure.State
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

module Game.Adventure.State where

import qualified Data.Map as M

import Game.Adventure.Command

data Location = Location Int Int deriving (Show, Eq, Ord)

center :: Location
center = Location 0 0

move :: Direction -> Location -> Location
move North (Location x y) = Location x (y - 1)
move South (Location x y) = Location x (y + 1)
move East (Location x y) = Location (x + 1) y
move West (Location x y) = Location (x - 1) y

distance :: Location -> Location -> Int
distance (Location x1 y1) (Location x2 y2) = abs (x1 - x2) + abs (y1 - y2)

data GameState item = GameState
  { inventory       :: [item]
  , items           :: M.Map Location [item]
  , location        :: Location
  } deriving (Show, Eq)

initialState :: GameState item
initialState = GameState
  { inventory = []
  , items = M.empty
  , location = center }

setInventory :: [item] -> GameState item -> GameState item
setInventory m st = st { inventory = m }

setItems :: M.Map Location [item] -> GameState item -> GameState item
setItems locations st = st { items = locations }

setLocation :: Location -> GameState item -> GameState item
setLocation loc st = st { location = loc }

modifyInventory :: ([item] -> [item]) -> GameState item -> GameState item
modifyInventory f st = st { inventory = f (inventory st) }

modifyItems :: (M.Map Location [item] -> M.Map Location [item]) -> GameState item -> GameState item
modifyItems f st = st { items = f (items st) }

modifyLocation :: (Location -> Location) -> GameState item -> GameState item
modifyLocation f st = st { location = f (location st) }
