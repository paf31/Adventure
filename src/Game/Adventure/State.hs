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

data GameState player item = GameState
  { inventories     :: M.Map player [item]
  , items           :: M.Map Location [item]
  , playerLocations :: M.Map player Location
  } deriving (Show, Eq)

initialState :: GameState player item
initialState = GameState
  { inventories = M.empty
  , items = M.empty
  , playerLocations = M.empty }

setInventories :: M.Map player [item] -> GameState player item -> GameState player item
setInventories m st = st { inventories = m }

setItems :: M.Map Location [item] -> GameState player item -> GameState player item
setItems locations st = st { items = locations }

setPlayerLocations :: M.Map player Location -> GameState player item -> GameState player item
setPlayerLocations locations st = st { playerLocations = locations }

modifyInventories :: (M.Map player [item] -> M.Map player [item]) -> GameState player item -> GameState player item
modifyInventories f st = st { inventories = f (inventories st) }

modifyItems :: (M.Map Location [item] -> M.Map Location [item]) -> GameState player item -> GameState player item
modifyItems f st = st { items = f (items st) }

modifyPlayerLocations :: (M.Map player Location -> M.Map player Location) -> GameState player item -> GameState player item
modifyPlayerLocations f st = st { playerLocations = f (playerLocations st) }
