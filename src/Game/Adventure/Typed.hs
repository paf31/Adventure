-----------------------------------------------------------------------------
--
-- Module      :  Game.Adventure.Typed
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

module Game.Adventure.Typed (
  Term,
  Type(..),
  typedRoom,
  var,
  ctor
) where

import Data.Maybe (fromMaybe)

import Control.Monad
import Control.Applicative

import Control.Monad.State

import qualified Data.Map as M

import Game.Adventure.Parser
import Game.Adventure.Actions
import Game.Adventure.Room

data ConType = Ctor | Var deriving Eq

data Type
  = TyCon String ConType
  | TyApp Type Type
  | TyArr Type Type deriving Eq

instance Show Type where
  show (TyCon s _) = s
  show (TyApp t1@(TyCon _ _) t2@(TyCon _ _)) = show t1 ++ " " ++ show t2
  show (TyApp t1@(TyApp _ _) t2@(TyCon _ _)) = show t1 ++ " " ++ show t2
  show (TyApp t1@(TyCon _ _) t2) = show t1 ++ " (" ++ show t2 ++ ")"
  show (TyApp t1@(TyApp _ _) t2) = show t1 ++ " (" ++ show t2 ++ ")"
  show (TyApp t1 t2) = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (TyArr t1@(TyArr _ _) t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (TyArr t1 t2) = show t1 ++ " -> " ++ show t2

type Term = String

freshName :: String -> State (Int, M.Map String String) String
freshName var = do
  (n, m) <- get
  let name = var ++ show n
  put $ (n + 1, M.insert var name m)
  return name

renameVar :: String -> State (Int, M.Map String String) String
renameVar var = do
  (_, m) <- get
  case M.lookup var m of
    Nothing -> freshName var
    Just name -> return name

var :: String -> Type
var = flip TyCon Var

ctor :: String -> Type
ctor = flip TyCon Ctor

renameAllVars :: Type -> State (Int, M.Map String String) Type
renameAllVars ctor@(TyCon _ Ctor) = return ctor
renameAllVars (TyCon name Var) = var <$> renameVar name
renameAllVars (TyApp t1 t2) = TyApp <$> renameAllVars t1 <*> renameAllVars t2
renameAllVars (TyArr t1 t2) = TyArr <$> renameAllVars t1 <*> renameAllVars t2

unify :: Type -> Type -> Either String [(String, Type)]
unify = unify' []
  where
  unify' env (TyCon var Var) ty = case lookup var env of
    Nothing -> return $ (var, ty) : env
    Just ty' -> (++) <$> pure env <*> unify ty ty'
  unify' env ty (TyCon var Var) = case lookup var env of
    Nothing -> return $ (var, ty) : env
    Just ty' -> (++) <$> pure env <*> unify ty ty'
  unify' env (TyCon c1 Ctor) (TyCon c2 Ctor) | c1 == c2 = Right env
  unify' env (TyApp t1 t2) (TyApp t3 t4) = do env' <- unify' env t1 t3
                                              env'' <- unify' env' t2 t4
                                              return $ env' ++ env''
  unify' env (TyArr t1 t2) (TyArr t3 t4) = do env' <- unify' env t1 t3
                                              env'' <- unify' env' t2 t4
                                              return $ env' ++ env''
  unify' _ t1 t2 = Left $ "Cannot match " ++ show t1 ++ " with " ++ show t2 ++ "."

substitute :: [(String, Type)] -> Type -> Type
substitute _ ctor@(TyCon _ Ctor) = ctor
substitute m (TyCon name Var) = fromMaybe (var name) (lookup name m)
substitute m (TyApp t1 t2) = TyApp (substitute m t1) (substitute m t2)
substitute m (TyArr t1 t2) = TyArr (substitute m t1) (substitute m t2)

typedRoom :: Location -> String -> (forall m. (MonadGame Term m) => CommandParser (R Term m ())) -> [Term] -> [(Term, Type)] -> Room Term
typedRoom loc desc run items env = standardRoom loc desc str run' items env
  where
    run' env = msum
      [ apply env <$> (match "let" *> str) <*> (match "=" *> str) <*> str
      , typeOf env <$> (match ":t" *> str)
      , fmap (\act -> act >> return env) run
      ]
    apply :: (MonadGame Term m) => [(Term, Type)] -> Term -> Term -> Term -> R Term m [(Term, Type)]
    apply env name f x =
      case lookup name env of
        Just _ -> showMessage "Already defined" >> return env
        Nothing -> case checkTypes <$> lookup f env <*> lookup x env of
          Nothing -> showMessage "Undefined" >> return env
          Just (Left err) -> showMessage err >> return env
          Just (Right ty) -> do
            addToRoom name
            let newEnv = (name, ty) : env
            typeOf newEnv name
    typeOf :: (MonadGame Term m) => [(Term, Type)] -> Term -> R Term m [(Term, Type)]
    typeOf env term = case lookup term env of
      Nothing -> showMessage "Undefined" >> return env
      Just ty -> showMessage (term ++ " :: " ++ show ty) >> return env
    checkTypes :: Type -> Type -> Either String Type
    checkTypes (TyArr t1 t2) t3 =
      let
        (t1', t2') = evalState ((,) <$> renameAllVars t1 <*> renameAllVars t2) (0, M.empty)
      in case unify t1' t3 of
        Left err -> Left err
        Right map -> Right $ substitute map t2'
    checkTypes t1 t2 = Left $ "Cannot apply type " ++ show t1 ++ " to type " ++ show t2
