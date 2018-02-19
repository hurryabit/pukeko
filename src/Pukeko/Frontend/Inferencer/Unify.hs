{-# LANGUAGE BangPatterns #-}
module Pukeko.FrontEnd.Inferencer.Unify
  ( TU
  , unify
  )
where

import Pukeko.Prelude

import           Control.Monad.Freer.Supply
import           Control.Monad.ST
import           Data.STRef

import           Pukeko.Pretty
import           Pukeko.AST.Name
import           Pukeko.FrontEnd.Inferencer.Gamma
import           Pukeko.FrontEnd.Inferencer.UType
import           Pukeko.FrontEnd.Info

type TU s ev = EffGamma s ev
  [Reader ModuleInfo, Reader SourcePos, Supply UVarId, Error Failure, NameSource, ST s]

-- TODO: link compression
-- | Unwind a chain of 'ULink's.
unwind :: UType s tv -> TU s ev (UType s tv)
unwind t0 = case t0 of
  UVar uref -> do
    sendM (readSTRef uref) >>= \case
      ULink t1 -> unwind t1
      UFree{}  -> pure t0
  _ -> pure t0

-- | Read a 'UVar' /after/ 'unwind'ing.
readUnwound :: STRef s (UVar s tv) -> TU s ev (UVarId, Level)
readUnwound uref =
  sendM (readSTRef uref) >>= \case
    ULink{} -> impossible  -- we only call this after unwinding
    UFree v lvl -> pure (v, lvl)

-- | Perform the occurs check. Assumes that the 'UVar' has been unwound.
occursCheck :: STRef s (UVar s tv) -> UType s tv -> TU s ev ()
occursCheck uref1 t2 = case t2 of
  UVar uref2
    | uref1 == uref2 -> throwHere "occurs check"
    | otherwise -> do
        sendM (readSTRef uref2) >>= \case
          UFree x2 l2 -> do
            (_, l1) <- readUnwound uref1
            sendM (writeSTRef uref2 (UFree x2 (min l1 l2)))
          ULink t2' -> occursCheck uref1 t2'
  UTVar v -> do
    !_ <- lookupTVar v  -- NOTE: This is a sanity check.
    pure ()
  UTAtm{} -> pure ()
  UTUni{} -> impossible  -- UVar is assumed to be unwound
  UTApp tf tp -> occursCheck uref1 tf *> occursCheck uref1 tp

unify :: UType s tv -> UType s tv -> TU s ev ()
unify t1 t2 = do
  t1 <- unwind t1
  t2 <- unwind t2
  case (t1, t2) of
    (UVar uref1, UVar uref2) | uref1 == uref2 -> return ()
    (UVar uref1, _) -> do
      (v1, _) <- readUnwound uref1
      occursCheck uref1 t2 `catchError` \(_ :: Failure) -> do
        p2 <- sendM (prettyUType 0 t2)
        throwHere (quotes (pretty v1) <+> "occurs in" <+> p2)
      sendM (writeSTRef uref1 (ULink t2))
    (_, UVar _) -> unify t2 t1
    (UTVar x1, UTVar x2)
      | x1 == x2 -> pure ()
    (UTAtm atom1, UTAtm atom2)
      | atom1 == atom2 -> pure ()
    (UTApp tf1 tp1, UTApp tf2 tp2) -> unify tf1 tf2 *> unify tp1 tp2
    _ -> do
      p1 <- sendM (prettyUType 0 t1)
      p2 <- sendM (prettyUType 0 t2)
      throwHere ("mismatching types" <+> p1 <+> "and" <+> p2)
