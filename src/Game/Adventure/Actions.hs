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

import Game.Adventure.Parser

import Control.Arrow ((&&&))

-- State

type Location = String

data GameState item = GameState
  { inventory       :: [item]
  , location        :: Location
  } deriving (Show, Eq)

data Room item = Room
  { name :: Location
  , description :: String
  , runRoom :: (MonadGame item m) => CommandParser (m (Room item)) }

data AllState item = AllState
  { globalState :: GameState item
  , rooms :: M.Map Location (Room item) }

initialState :: [Room item] -> Room item -> AllState item
initialState rooms room = AllState (GameState [] (name room)) (M.fromList (map (name &&& id) rooms))

setInventory :: [item] -> GameState item -> GameState item
setInventory m st = st { inventory = m }

setLocation :: Location -> GameState item -> GameState item
setLocation loc st = st { location = loc }

modifyInventory :: ([item] -> [item]) -> GameState item -> GameState item
modifyInventory f st = st { inventory = f (inventory st) }

-- Rooms

roomState :: Location -> String -> (forall m. (MonadGame item m) => s -> CommandParser (m (s, String))) -> s -> Room item
roomState loc desc f s = Room loc desc $ fmap (fmap (\(s', desc') -> roomState loc desc' f s')) (f s)

-- Reset Monoid

newtype Reset w = Reset { runReset :: Maybe w } deriving (Show, Read, Eq, Ord)

instance (Monoid w) => Monoid (Reset w) where
  mempty = Reset (Just mempty)
  Reset (Just w1) `mappend` Reset (Just w2) = Reset (Just (w1 `mappend` w2))
  _ `mappend` r@(Reset (Just _)) = r
  _ `mappend` _ = mempty

-- Game Monad

type MonadGame item m = (Functor m, Monad m, S.MonadState (GameState item) m, W.MonadWriter (Reset [String]) m)

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

with :: (MonadGame item m, Eq item, Show item) => item -> m a -> m (Maybe a)
with item action = do
  inInventory <- has item
  if inInventory
  then
    fmap Just action
  else do
    showMessage $ "You do not have '" ++ show item ++ "'."
    return Nothing

without :: (MonadGame item m, Eq item, Show item) => item -> m a -> m (Maybe a)
without item action = do
  inInventory <- has item
  if inInventory
  then do
    showMessage $ "You cannot do that if you have '" ++ show item ++ "'."
    return Nothing
  else
    fmap Just action

addToInventory :: (MonadGame item m, Eq item, Show item) => item -> m ()
addToInventory item = S.modify $ modifyInventory $ (:) item

removeFromInventory :: (MonadGame item m, Eq item, Show item) => item -> m ()
removeFromInventory item = S.modify $ modifyInventory $ delete item

look :: (Show item, MonadGame item m) => Room item -> m ()
look room = do
  st <- S.get
  location <- currentLocation
  showMessage (description room)
  flip mapM_ (inventory st) $ \item -> showMessage $ "You have '" ++ show item ++ "'."

-- Player Location

currentLocation :: (MonadGame item m) => m Location
currentLocation = S.get >>= return . location

moveTo :: (MonadGame item m) => Room item -> m ()
moveTo = S.modify . setLocation . name

-- Standard Actions

standardCommands :: (Show item, MonadGame item m) => Room item -> CommandParser (m ())
standardCommands room = match "look" >> return (look room)
