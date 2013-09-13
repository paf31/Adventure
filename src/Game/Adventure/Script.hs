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

type MonadGame player item m = (MonadState (GameState player item) m, MonadWriter [String] m)

data Script player item = Script
  { initialize :: (MonadGame player item m) => m ()
  , step       :: (MonadGame player item m) => player -> Location -> CommandParser (m ())
  , describe   :: (MonadGame player item m) => player -> Location -> Maybe (m String)
  }

instance Monoid (Script player item) where
  mempty = Script
    { initialize = return ()
    , step = const $ const mzero
    , describe = const $ const Nothing
    }
  mappend s1 s2 = Script
    { initialize = initialize s1 >> initialize s2
    , step = \p l -> step s1 p l `mplus` step s2 p l
    , describe = \p l -> describe s1 p l `mplus` describe s2 p l
    }

initializeWith :: (forall m. (MonadGame player item m) => m ()) -> Script player item
initializeWith action = mempty { initialize = action }

anywhere :: (forall m. (MonadGame player item m) => player -> CommandParser (m ())) -> Script player item
anywhere parser = mempty { step = \player _ -> parser player }

room :: Location -> (forall m. (MonadGame player item m) => player -> m String) ->
                    (forall m. (MonadGame player item m) => player -> CommandParser (m ())) ->
                    Script player item
room loc name parser = mempty
  { step = step'
  , describe = describe'
  }
  where
    step' player loc' | loc' == loc = parser player
    step' _ _ = mzero
    describe' player loc' | loc' == loc = Just (name player)
    describe' _ _ = Nothing
