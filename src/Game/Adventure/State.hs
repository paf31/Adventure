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

type Location = String

data GameState item = GameState
  { inventory       :: [item]
  , items           :: M.Map Location [item]
  , location        :: Location
  } deriving (Show, Eq)

initialState :: Location -> GameState item
initialState = GameState [] M.empty

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
