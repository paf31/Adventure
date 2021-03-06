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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Game.Adventure.Actions where

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans
import qualified Data.Map as M
import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W

import Game.Adventure.State
import Game.Adventure.Parser

-- Game Monad

newtype Reset w = Reset { runReset :: Maybe w } deriving (Show, Read, Eq, Ord)

instance (Monoid w) => Monoid (Reset w) where
  mempty = Reset (Just mempty)
  Reset (Just w1) `mappend` Reset (Just w2) = Reset (Just (w1 `mappend` w2))
  _ `mappend` r@(Reset (Just _)) = r
  _ `mappend` _ = mempty

type MonadGame item m = (S.MonadState (GameState item) m, W.MonadWriter (Reset [String]) m)

data Room item = Room
  { name         :: Location
  , initialItems :: [item]
  , description  :: (MonadGame item m) => m String
  , step         :: (MonadGame item m) => CommandParser (m ())
  }

-- Text

showMessage  :: (MonadGame item m) => String -> m ()
showMessage = W.tell . Reset . Just . return

clearLog :: (MonadGame item m) => m ()
clearLog = W.tell mempty

-- Inventory Functions

has :: (MonadGame item m, Eq item) => item -> m Bool
has item = do
  inv <- S.get >>= return . inventory
  return $ item `elem` inv

with :: (MonadGame item m, Eq item, Show item) => item -> m () -> m ()
with item action = do
  inInventory <- has item
  if inInventory
  then
    action
  else
    showMessage $ "You do not have '" ++ show item ++ "'."

without :: (MonadGame item m, Eq item, Show item) => item -> m () -> m ()
without item action = do
  inInventory <- has item
  if inInventory
  then do
    showMessage $ "You cannot do that if you have '" ++ show item ++ "'."
  else
    action

addToInventory :: (MonadGame item m, Eq item, Show item) => item -> m ()
addToInventory item = S.modify $ modifyInventory $ (:) item

removeFromInventory :: (MonadGame item m, Eq item, Show item) => item -> m ()
removeFromInventory item = S.modify $ modifyInventory $ delete item

pickUp :: (MonadGame item m, Eq item, Ord item, Show item) => item -> m ()
pickUp item = do
  location <- currentLocation
  itemIsHere <- isItemAtLocation location item
  if itemIsHere
  then do
    addToInventory  item
    removeItemAt location item
    showMessage $ "You now have '" ++ show item ++ "'. "
  else
    showMessage "That item is not here."

putDown :: (MonadGame item m, Eq item, Ord item, Show item) => item -> m ()
putDown item = do
  location <- currentLocation
  with item $ do
    removeFromInventory item
    addItemAt location item
    showMessage $ "You put down '" ++ show item ++ "'. "

pickUpIfNotInInventory :: (MonadGame item m, Ord item, Eq item, Show item) => item -> m ()
pickUpIfNotInInventory  item = without  item $ pickUp  item

look :: (Show item, Ord item, MonadGame item m) => Room item -> m ()
look room = do
  st <- S.get
  location <- currentLocation
  itemsVisible <- itemsAtCurrentLocation
  description room >>= showMessage
  flip mapM_ (inventory st) $ \item -> showMessage $ "You have '" ++ show item ++ "'."
  flip mapM_ itemsVisible $ \item -> showMessage $ "You can see '" ++ show item ++ "'."

-- Player Location

currentLocation :: (MonadGame item m) => m Location
currentLocation = S.get >>= return . location

moveTo :: (MonadGame item m) => Room item -> m ()
moveTo = S.modify . setLocation . name

-- Item Locations

isItemAtLocation :: (MonadGame item m, Eq item) => Location -> item -> m Bool
isItemAtLocation location item = S.get >>= return . maybe False (elem item) . M.lookup location . items

itemsAtCurrentLocation :: (MonadGame item m, Ord item) => m [item]
itemsAtCurrentLocation = do
  location <- currentLocation
  st <- S.get
  return $ maybe [] id . M.lookup location . items $ st

addItemAt :: (MonadGame item m, Ord item) => Location -> item -> m ()
addItemAt location item = S.modify $ modifyItems $ M.alter (maybe (Just [item]) $ Just . insert item) location

removeItemAt :: (MonadGame item m, Ord item) => Location -> item -> m ()
removeItemAt location item = S.modify $ modifyItems $ M.adjust (delete item) location
