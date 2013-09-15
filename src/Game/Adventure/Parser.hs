-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure.Parser
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Adventure.Parser (
    CommandParser(..),
    match,
    matchAny,
    str,
    evalCommandParser
) where

import Data.Char
import Data.List
import Data.List.Split
import Control.Monad
import Control.Applicative
import qualified Control.Monad.State as S
import qualified Data.Map as M

import Game.Adventure.Command

newtype CommandParser a = CommandParser { runCommandParser :: S.StateT [String] Maybe a } deriving (Functor, Applicative, Monad, S.MonadState [String], MonadPlus, Alternative)

match :: String -> CommandParser ()
match s = str >>= guard . ((==) (map toUpper s) . map toUpper)

matchAny :: [(String, a)] -> CommandParser a
matchAny = foldl (<|>) mzero . map matchOne
  where matchOne (s, a) = match s >> return a

str :: CommandParser String
str = do
  ss <- S.get
  case ss of
    (s:ss') -> S.put(ss') >> return s
    _ -> mzero

evalCommandParser :: CommandParser a -> String -> Maybe a
evalCommandParser p = S.evalStateT (runCommandParser p) . splitOneOf " \t"
