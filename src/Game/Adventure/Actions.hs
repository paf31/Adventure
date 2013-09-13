-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure.Actions
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

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Game.Adventure.Actions where

import Data.List
import Control.Monad.Trans
import qualified Data.Map as M
import qualified Control.Monad.State as S

import Game.Adventure.State
import Game.Adventure.Command

-- Text

class (Monad m) => MonadText m where
  showMessage :: String -> m ()

instance (MonadText m) => MonadText (S.StateT s m) where
  showMessage = lift . showMessage

-- Inventory Functions

inventory :: (S.MonadState (GameState player item) m, Ord player) => player -> m [item]
inventory p = do
  st <- S.get
  return $ maybe [] id . M.lookup p . inventories $ st

has :: (S.MonadState (GameState player item) m, Ord player, Eq item) => player -> item -> m Bool
has player item = do
  inv <- inventory player
  return $ item `elem` inv

with :: (MonadText m, S.MonadState (GameState player item) m, Ord player, Eq item, Show item) => player -> item -> m () -> m ()
with player item action = do
  inInventory <- has player item
  if inInventory
  then
    action
  else
    showMessage $ "You do not have '" ++ show item ++ "'."

without :: (MonadText m, S.MonadState (GameState player item) m, Ord player, Eq item, Show item) => player -> item -> m () -> m ()
without player item action = do
  inInventory <- has player item
  if inInventory
  then do
    showMessage $ "You cannot do that if you have '" ++ show item ++ "'."
  else
    action

addToInventory :: (S.MonadState (GameState player item) m, Ord player, Eq item, Show item) => player -> item -> m ()
addToInventory player item = S.modify $ modifyInventories $ M.alter (maybe (Just [item]) (Just . (:) item)) player

removeFromInventory :: (S.MonadState (GameState player item) m, Ord player, Eq item, Show item) => player -> item -> m ()
removeFromInventory player item = S.modify $ modifyInventories $ M.adjust (delete item) player

pickUp :: (MonadText m, S.MonadState (GameState player item) m, Ord player, Eq item, Ord item, Show item) => player -> item -> m ()
pickUp player item = do
  location <- currentLocation player
  itemIsHere <- isItemAtLocation location item
  if itemIsHere
  then do
    addToInventory player item
    showMessage $ "You now have '" ++ show item ++ "'. "
  else
    showMessage "That item is not here."

putDown :: (MonadText m, S.MonadState (GameState player item) m, Ord player, Eq item, Ord item, Show item) => player -> item -> m ()
putDown player item = do
  location <- currentLocation player
  with player item $ do
    removeFromInventory player item
    showMessage $ "You no longer have '" ++ show item ++ "'. "

pickUpIfNotInInventory :: (MonadText m, S.MonadState (GameState player item) m, Ord player, Ord item, Eq item, Show item) => player -> item -> m ()
pickUpIfNotInInventory player item = without player item $ pickUp player item

-- Player Locations

currentLocation :: (S.MonadState (GameState player item) m, Ord player) => player -> m Location
currentLocation p = S.get >>= return . maybe center id . M.lookup p . playerLocations

moveTo :: (S.MonadState (GameState player item) m, Ord player) => player -> Location -> m ()
moveTo player location = S.modify $ modifyPlayerLocations $ M.insert player location

moveInDirection :: (S.MonadState (GameState player item) m, Ord player) => player -> Direction -> m ()
moveInDirection player direction = currentLocation player >>= moveTo player . move direction

-- Item Locations

isItemAtLocation :: (S.MonadState (GameState player item) m, Eq item) => Location -> item -> m Bool
isItemAtLocation location item = S.get >>= return . maybe False (elem item) . M.lookup location . items

itemsAtCurrentLocation :: (S.MonadState (GameState player item) m, Ord player, Ord item) => player -> m [item]
itemsAtCurrentLocation p = do
  location <- currentLocation p
  st <- S.get
  return $ maybe [] id . M.lookup location . items $ st

addItemAt :: (S.MonadState (GameState player item) m, Ord item) => Location -> item -> m ()
addItemAt location item = S.modify $ modifyItems $ M.alter (maybe (Just [item]) $ Just . insert item) location

removeItemAt :: (S.MonadState (GameState player item) m, Ord item) => Location -> item -> m ()
removeItemAt location item = S.modify $ modifyItems $ M.adjust (delete item) location
