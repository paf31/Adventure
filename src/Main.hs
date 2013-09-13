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

data Player = Ego deriving (Show, Eq, Ord)

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

firstRoom :: (Ord player) => Script player Item
firstRoom = room center description $ \player -> do
  match "light" >> (match "fire" <|> match "wood" <|> match "firewood" <|> match "matches") >> (return $ do
    with player Matches $ with player (Firewood Unlit) $ do
      showMessage "You light the firewood with the matches."
      removeFromInventory player Matches
      removeFromInventory player (Firewood Unlit)
      addToInventory player (Firewood Lit))
  where
  description player = do
    lit <- has player (Firewood Lit)
    if lit
    then return "the kitchen"
    else return "a dimly lit room"

courtyard :: (Ord player) => Script player Item
courtyard = room (move North center) (const $ return "a courtyard with a fountain") $ \player -> msum
  [ match "fill" >> match "Bucket" >> (return $ do
    with player (Bucket Empty) $ do
      showMessage "You fill the bucket at the fountain"
      removeFromInventory player (Bucket Empty)
      addToInventory player (Bucket Full))
  ]

script :: (Ord player) => Script player Item
script = mconcat
  [ initializeWith $ do
      mapM_ (addItemAt center) [Matches, Firewood Unlit]
      addItemAt (move North center) (Bucket Empty)
  , firstRoom
  , courtyard
  , anywhere $ \player -> match "use" >> match "Bucket" >> (return $ do
      with player (Bucket Full) $ with player (Firewood Lit) $ do
        showMessage "You extinguish the flames."
        removeFromInventory player (Bucket Full)
        removeFromInventory player (Firewood Lit))
  ]

main :: IO ()
main = singlePlayer item Ego script
