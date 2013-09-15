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
kitchen = Room "kitchen" [Matches, Firewood Unlit] description $ msum
  [ match "light" >> (match "fire" <|> match "wood" <|> match "firewood" <|> match "matches") >> return (do
      with Matches $ with (Firewood Unlit) $ do
        showMessage "You light the firewood with the matches."
        removeFromInventory Matches
        removeFromInventory (Firewood Unlit)
        addToInventory (Firewood Lit))
  , match "move" >> match "north" >> return (moveTo courtyard)
  ]
  where
  description :: (MonadGame Item m) => m String
  description = do
    lit <- has (Firewood Lit)
    if lit
    then return "You are in the kitchen. You see a courtyard to the north."
    else return "You are in a dimly lit room."

courtyard :: Room Item
courtyard = Room "courtyard" [Bucket Empty] (return "You are in a courtyard with a fountain.") $ msum
  [ match "fill" >> match "Bucket" >> return (do
      with (Bucket Empty) $ do
        showMessage "You fill the bucket at the fountain"
        removeFromInventory (Bucket Empty)
        addToInventory (Bucket Full))
  , match "use" >> match "Bucket" >> return (do
      with (Bucket Full) $ with (Firewood Lit) $ do
        showMessage "You extinguish the flames."
        removeFromInventory (Bucket Full)
        removeFromInventory (Firewood Lit))
  , match "move" >> match "south" >> return (moveTo kitchen)
  ]

main :: IO ()
main = singlePlayer item [kitchen, courtyard] kitchen
