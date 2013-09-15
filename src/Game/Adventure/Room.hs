-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure.Room
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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Adventure.Room (

  standardRoom,

  setRoomDescription,
  addToRoom,
  removeFromRoom,
  itemsInRoom

) where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import Data.Maybe (fromMaybe)
import Data.Function (fix)
import Data.List (find, delete, intercalate)

import qualified Data.Map as M

import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W

import Game.Adventure.Parser
import Game.Adventure.Actions

-- R Monad

newtype R item m a = R { unR :: S.StateT ([item], String) m a } deriving (Functor, Monad)

instance (S.MonadState (GameState item) m) => (S.MonadState (GameState item) (R item m)) where
  get = R $ lift S.get
  put gs = R $ lift $ S.put gs

deriving instance (W.MonadWriter (Reset [String]) m) => (W.MonadWriter (Reset [String]) (R item m))

runR :: R item m a -> [item] -> String -> m (a, ([item], String))
runR r is s = S.runStateT (unR r) (is, s)

setRoomDescription :: (Monad m) => String -> R item m ()
setRoomDescription s = R $ S.modify (\(is, _) -> (is, s))

addToRoom :: (Monad m) => item -> R item m ()
addToRoom i = R $ S.modify (\(is, d) -> (i:is, d))

removeFromRoom :: (Monad m, Eq item) => item -> R item m ()
removeFromRoom i = R $ S.modify (\(is, d) -> (delete i is, d))

itemsInRoom :: (Functor m, Monad m) => R item m [item]
itemsInRoom = R $ fmap fst S.get

-- Rooms

standardRoom :: (Show item, Eq item) => Location -> String -> CommandParser item -> (forall m. (MonadGame item m) => s -> CommandParser (R item m s)) -> [item] -> s -> Room item
standardRoom loc desc item run items s = roomState loc (mkDesc items) (\(items, s) ->
    fmap (\r -> do { (s', (items', desc')) <- runR r items desc; return ((items', s'), desc') })
      (standardRoomCommands item items s <|> (run s)))
      (items, s)
  where
    standardRoomCommands :: (MonadGame item m, Show item, Eq item) => CommandParser item -> [item] -> s -> CommandParser (R item m s)
    standardRoomCommands item items s = msum
      [ pickUp s <$> (match "take" *> item)
      , putDown s <$> (match "drop" *> item)
      ]
    pickUp :: (MonadGame item m, Show item, Eq item) => s -> item -> R item m s
    pickUp s item = removeFromRoom item >> addToInventory item >> updateDescription >> return s
    putDown :: (MonadGame item m, Show item, Eq item) => s -> item -> R item m s
    putDown s item = addToRoom item >> removeFromInventory item >> updateDescription >> return s
    updateDescription :: (Functor m, Monad m, Show item) => R item m ()
    updateDescription = do
       items <- itemsInRoom
       setRoomDescription $ mkDesc items
    mkDesc :: (Show item) => [item] -> String
    mkDesc items = unlines $ desc : map (("You can see " ++) . (++ ".") . show) items
