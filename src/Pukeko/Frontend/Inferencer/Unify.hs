{-# LANGUAGE BangPatterns #-}
module Pukeko.FrontEnd.Inferencer.Unify
  ( TU
  , unify
  )
where

import Pukeko.Prelude

import           Control.Monad.ST
import           Control.Monad.ST.Class
import           Data.STRef

import           Pukeko.Pretty
import qualified Pukeko.AST.Identifier            as Id
import           Pukeko.FrontEnd.Inferencer.Gamma
import           Pukeko.FrontEnd.Inferencer.UType
import           Pukeko.FrontEnd.Info

type TU s ev = GammaT s ev (InfoT (ExceptT Doc (ST s)))

-- TODO: link compression
unwind :: UType s tv -> TU s ev (UType s tv)
unwind t0 = case t0 of
  UVar uref -> do
    uvar <- liftST $ readSTRef uref
    case uvar of
      ULink t1  -> unwind t1
      UFree _ _ -> return t0
  _ -> return t0

readUnwound :: STRef s (UVar s tv) -> TU s ev (Id.TVar, Int)
readUnwound uref = do
  uvar <- liftST (readSTRef uref)
  case uvar of
    ULink{} -> bug "link in unwound unification variable"
    UFree v lvl -> pure (v, lvl)

-- NOTE: The reference is assumed to be unwound.
occursCheck :: STRef s (UVar s tv) -> UType s tv -> TU s ev ()
occursCheck uref1 t2 = case t2 of
  UVar uref2
    | uref1 == uref2 -> throwHere "occurs check"
    | otherwise      -> do
        uvar2 <- liftST $ readSTRef uref2
        case uvar2 of
          UFree x2 l2 -> do
            (_, l1) <- readUnwound uref1
            liftST $ writeSTRef uref2 (UFree x2 (min l1 l2))
          ULink t2' -> occursCheck uref1 t2'
  UTVar v -> do
    !_ <- lookupTVar v  -- NOTE: This is a sanity check.
    pure ()
  UTCon _ -> pure ()
  UTArr   -> pure ()
  UTUni{} -> bug "universal quantification in occurs check"
  UTApp tf tp -> occursCheck uref1 tf *> occursCheck uref1 tp

unify :: UType s tv -> UType s tv -> TU s ev ()
unify t1 t2 = do
  t1 <- unwind t1
  t2 <- unwind t2
  case (t1, t2) of
    (UVar uref1, UVar uref2) | uref1 == uref2 -> return ()
    (UVar uref1, _) -> do
      (v1, _) <- readUnwound uref1
      occursCheck uref1 t2 `catchError` \_ -> do
        p2 <- liftST $ prettyUType prettyNormal 0 t2
        throwHere (quotes (pretty v1) <+> "occurs in" <+> p2)
      liftST $ writeSTRef uref1 (ULink t2)
    (_, UVar _) -> unify t2 t1
    (UTVar x1, UTVar x2)
      | x1 == x2 -> pure ()
    (UTArr, UTArr) -> pure ()
    (UTCon tcon1, UTCon tcon2)
      | tcon1 == tcon2 -> pure ()
    (UTApp tf1 tp1, UTApp tf2 tp2) -> unify tf1 tf2 *> unify tp1 tp2
    _ -> do
      p1 <- liftST $ prettyUType prettyNormal 0 t1
      p2 <- liftST $ prettyUType prettyNormal 0 t2
      throwHere ("mismatching types" <+> p1 <+> "and" <+> p2)
