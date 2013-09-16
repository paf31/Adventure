-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure.Slides
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

module Game.Adventure.Slides (
  slides
) where

import Control.Applicative

import Game.Adventure

slide1 :: Room Term
slide1 = typedRoom "slide1" "A room full of monads" empty ["return", ">>="]
  [ ("return", TyArr (var "a") (TyApp (var "m") (var "a")))
  , (">>=", TyArr (TyApp (var "m") (var "a")) (TyArr (TyArr (var "a") (TyApp (var "m") (var "b"))) (TyApp (var "m") (var "b"))))
  ]

slides :: IO ()
slides = play str (initialState [slide1] slide1)


