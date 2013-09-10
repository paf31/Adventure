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

module Main where

import Game.Adventure

data Player = Ego deriving (Show, Eq, Ord)

data Item = Matches | Bucket | Kindling deriving (Show, Eq, Ord)

item :: CommandParser Item
item = matchAny [ ("Matches", Matches), ("Bucket", Bucket), ("Kindling", Kindling) ]

-- Example Game

script :: Script Player Item
script = do
  mapM_ (addItemAt center) [Matches, Bucket, Kindling]
  return $ \player cmd -> do
    case cmd of
      Combine Matches Kindling ->
        with player Matches $ with player Kindling $ do
          showMessage "You set fire to the kindling. The door is on fire."
          putDown player Matches
          putDown player Kindling
      Use Bucket -> with player Bucket $ do
        showMessage "You extinguish the flames."
        putDown player Bucket
      _ -> showMessage "I don't know how to do that."

main :: IO ()
main = singlePlayer item Ego script
