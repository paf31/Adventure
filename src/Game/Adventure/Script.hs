-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure.Script
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Game.Adventure.Script where

import Data.Monoid
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

import Game.Adventure.Command
import Game.Adventure.State
import Game.Adventure.Parser

type MonadGame item m = (MonadState (GameState item) m, MonadWriter [String] m)

data Script item = Script
  { initialize :: (MonadGame item m) => m ()
  , step       :: (MonadGame item m) => Location -> CommandParser (m ())
  , describe   :: (MonadGame item m) => Location -> Maybe (m String)
  }

instance Monoid (Script item) where
  mempty = Script
    { initialize = return ()
    , step = const mzero
    , describe = const Nothing
    }
  mappend s1 s2 = Script
    { initialize = initialize s1 >> initialize s2
    , step = \l -> step s1 l `mplus` step s2 l
    , describe = \l -> describe s1 l `mplus` describe s2 l
    }

initializeWith :: (forall m. (MonadGame item m) => m ()) -> Script item
initializeWith action = mempty { initialize = action }

anywhere :: (forall m. (MonadGame item m) => CommandParser (m ())) -> Script item
anywhere parser = mempty { step = const parser }

room :: Location -> (forall m. (MonadGame item m) => m String) ->
                    (forall m. (MonadGame item m) => CommandParser (m ())) ->
                    Script item
room loc name parser = mempty
  { step = step'
  , describe = describe'
  }
  where
    step' loc' | loc' == loc = parser
    step' _ = mzero
    describe' loc' | loc' == loc = Just name
    describe' _ = Nothing
