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
    command,
    readCommand,
    match,
    matchAny,
    str
) where


import Data.List
import Data.List.Split
import Control.Monad
import Control.Applicative
import qualified Control.Monad.State as S
import qualified Data.Map as M

import Game.Adventure.Command

newtype CommandParser a = CommandParser { runCommandParser :: S.StateT [String] Maybe a } deriving (Functor, Applicative, Monad, S.MonadState [String], MonadPlus, Alternative)

match :: String -> CommandParser ()
match s = str >>= guard . (s ==)

matchAny :: [(String, a)] -> CommandParser a
matchAny = foldl (<|>) mzero . map matchOne
  where matchOne (s, a) = match s >> return a

str :: CommandParser String
str = do
  ss <- S.get
  case ss of
    (s:ss') -> S.put(ss') >> return s
    _ -> mzero

readCommand :: CommandParser a -> String -> Maybe a
readCommand p = S.evalStateT (runCommandParser p) . splitOneOf " \t"

direction :: CommandParser Direction
direction = matchAny [ ("North", North), ("East", East), ("South", South), ("West", West) ]

scriptCommand :: CommandParser item -> CommandParser (ScriptCommand item)
scriptCommand item = Combine <$> (match "combine" *> item) <*> (match "with" *> item)
                 <|> Use <$> (match "use" *> item)

systemCommand :: CommandParser item -> CommandParser (SystemCommand item)
systemCommand item = Move <$> (match "move" *> direction)
                 <|> Take <$> (match "take" *> item)
                 <|> matchAny [ ("look", Look), ("quit", Quit) ]

command :: CommandParser item -> CommandParser (Command item)
command item = Command <$> ((Left <$> systemCommand item) <|> (Right <$> scriptCommand item))
