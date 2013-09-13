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

module Main where

import Control.Monad
import Control.Applicative

import Game.Adventure

data Player = Ego deriving (Show, Eq, Ord)

data Lit = Lit | Unlit deriving (Show, Eq, Ord)

data Item = Matches | Bucket | Kindling Lit deriving (Eq, Ord)

instance Show Item where
  show (Kindling Lit) = "Burning Kindling"
  show (Kindling Unlit) = "Kindling"
  show Matches = "Matches"
  show Bucket = "Bucket"

item :: CommandParser Item
item = matchAny [ ("Matches", Matches), ("Bucket", Bucket), ("Kindling", Kindling Unlit), ("Burning Kindling", Kindling Lit) ]

-- Example Game

script :: (Ord player) => Script player Item
script = Script init step
  where
  init :: (MonadGame player Item m) => m ()
  init = mapM_ (addItemAt center) [Matches, Bucket, Kindling Unlit]
  step player = msum
    [ match "light" >> (match "Kindling" <|> match "Matches") >> (return $ do
        with player Matches $ with player (Kindling Unlit) $ do
          showMessage "You set fire to the kindling."
          removeFromInventory player Matches
          removeFromInventory player (Kindling Unlit)
          addToInventory player (Kindling Lit))
    , match "use" >> match "Bucket" >> (return $ do
        with player Bucket $ with player (Kindling Lit) $ do
          showMessage "You extinguish the flames."
          removeFromInventory player Bucket
          removeFromInventory player (Kindling Lit))
    ]

main :: IO ()
main = singlePlayer item Ego script
