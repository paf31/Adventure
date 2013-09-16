-----------------------------------------------------------------------------
--
-- Module      :  Main
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import Data.Monoid
import Control.Monad
import Control.Applicative

import Control.Arrow ((&&&))

import Game.Adventure

data Lit = Lit | Unlit deriving (Show, Eq, Ord)

data Full = Full | Empty deriving (Show, Eq, Ord)

data Item = Matches | Bucket Full | Firewood Lit deriving (Eq, Ord)

instance Show Item where
  show (Firewood Lit) = "Burning firewood"
  show (Firewood Unlit) = "Firewood"
  show Matches = "Matches"
  show (Bucket Full) = "Bucket full of water"
  show (Bucket Empty) = "Empty bucket"

item :: CommandParser Item
item = matchAny [ ("Matches", Matches), ("Bucket", Bucket Empty), ("Firewood", Firewood Unlit) ]

-- Example Game

kitchen :: Room Item
kitchen = standardRoom "kitchen" "You are in a dimly lit room." item (\_ -> msum
  [ match "light" >> (match "fire" <|> match "wood" <|> match "firewood" <|> match "matches") >> return (do
      fmap (const ()) $ with Matches $ with (Firewood Unlit) $ do
        showMessage "You light the firewood with the matches."
        removeFromInventory Matches
        removeFromInventory (Firewood Unlit)
        addToInventory (Firewood Lit)
        setRoomDescription "You are in the kitchen. You see a courtyard to the north.")
  , match "move" >> match "north" >> return (moveTo courtyard) ]
  ) [Matches, Firewood Unlit] ()

courtyard :: Room Item
courtyard = standardRoom "courtyard" "You are in a courtyard with a fountain." item (\_ -> msum
  [ match "fill" >> match "Bucket" >> return (do
      fmap (const ()) $ with (Bucket Empty) $ do
        showMessage "You fill the bucket at the fountain"
        removeFromInventory (Bucket Empty)
        addToInventory (Bucket Full))
  , match "use" >> match "Bucket" >> return (do
      fmap (const ()) $ with (Bucket Full) $ with (Firewood Lit) $ do
        showMessage "You extinguish the flames."
        removeFromInventory (Bucket Full)
        removeFromInventory (Firewood Lit))
  , match "move" >> match "south" >> return (moveTo kitchen) ]
  ) [Bucket Empty] ()

main :: IO ()
main = play item (initialState [kitchen, courtyard] kitchen)
