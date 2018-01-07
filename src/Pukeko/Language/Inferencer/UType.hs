{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.Language.Inferencer.UType
  ( UType (..)
  , UVar (..)
  , mkUTUni
  , unUTUni
  , (~>)
  , (*~>)
  , appN
  , appTCon
  , open
  , open1
  , subst
  , prettyUVar
  , prettyUType
  )
  where

import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Coerce       (coerce)
import           Data.Foldable     (toList)
import           Data.STRef
import qualified Data.List.NonEmpty as NE
import qualified Data.Map          as Map
import qualified Data.Vector.Sized as Vec

import           Pukeko.Error
import           Pukeko.Pretty
import           Pukeko.Language.Type (Type (..))
import qualified Pukeko.Language.Ident as Id
import           Pukeko.Language.AST.Scope

infixr 1 ~>, *~>

data UVar s tv
  = UFree Id.TVar Int  -- NOTE: The @Int@ is the level of this unification variable.
  | ULink (UType s tv)

data UType s tv
  = UTVar Id.TVar
  | UTArr
  | UTCon Id.TCon
  | UTApp (UType s tv) (UType s tv)
  | UTUni (NE.NonEmpty Id.TVar) (UType s tv)
  | UVar (STRef s (UVar s tv))

pattern UTFun :: UType s tv -> UType s tv -> UType s tv
pattern UTFun tx ty = UTApp (UTApp UTArr tx) ty

mkUTUni :: [Id.TVar] -> UType s tv -> UType s tv
mkUTUni xs0 t0 = case xs0 of
  [] -> t0
  x:xs -> UTUni (x NE.:| xs) t0

unUTUni :: UType s tv -> ([Id.TVar], UType s tv)
unUTUni = \case
  UTUni (x NE.:| xs) t -> (x:xs, t)
  t                    -> ([],   t)

(~>) :: UType s tv -> UType s tv -> UType s tv
(~>) = UTFun

(*~>) :: [UType s tv] -> UType s tv -> UType s tv
t_args *~> t_res = foldr (~>) t_res t_args

appN :: UType s tv -> [UType s tv] -> UType s tv
appN = foldl UTApp

appTCon :: Id.TCon -> [UType s tv] -> UType s tv
appTCon = appN . UTCon

open :: Type Void -> UType s Void
open = open1 . fmap absurd

open1 :: Type Id.TVar -> UType s Void
open1 = \case
  TVar x -> UTVar x
  TArr -> UTArr
  TCon c -> UTCon c
  TApp tf tp -> UTApp (open1 tf) (open1 tp)
  TUni xs tq -> mkUTUni (toList xs) (open1 (fmap (scope (xs Vec.!) id) tq))

subst :: Map.Map Id.TVar (UType s tv) -> UType s tv -> ST s (UType s tv)
subst env t = runReaderT (subst' t) env
  where
    subst' :: UType s tv
           -> ReaderT (Map.Map Id.TVar (UType s tv)) (ST s) (UType s tv)
    subst' = \case
      UTVar x -> do
        let e = bug "type instantiation" "unknown variable" (Just $ show x)
        Map.findWithDefault e x <$> ask
      t@UTArr     -> pure t
      t@(UTCon _) -> pure t
      UTApp tf tp -> UTApp <$> subst' tf <*> subst' tp
      UTUni{} -> bug "type inst" "universal quantification" Nothing
      t0@(UVar uref) -> do
        uvar <- lift $ readSTRef uref
        case uvar of
          UFree _ _ -> pure t0
          ULink t1  -> subst' t1

-- * Pretty printing
prettyUVar :: PrettyLevel -> Rational -> UVar s tv -> ST s Doc
prettyUVar lvl prec = \case
  UFree x _ -> pure (pretty x)
  ULink t   -> prettyUType lvl prec t

prettyUType :: PrettyLevel -> Rational -> UType s tv -> ST s Doc
prettyUType lvl prec = \case
  UTVar v -> pure (pretty v)
  UTArr   -> pure "(->)"
  UTCon c -> pure (pretty c)
  UTFun tx ty -> do
    px <- prettyUType lvl 2 tx
    py <- prettyUType lvl 1 ty
    pure $ maybeParens (prec > 1) $ px <+> "->" <+> py
  UTApp tf tx -> do
    pf <- prettyUType lvl 2 tf
    px <- prettyUType lvl 3 tx
    pure $ maybeParens (prec > 2) $ pf <+> px
  UTUni xs tq -> do
    pq <- prettyUType lvl 0 tq
    pure $ maybeParens (prec > 0) $ "∀" <> hsepMap pretty xs <> "." <+> pq
  UVar uref -> do
    uvar <- readSTRef uref
    prettyUVar lvl prec uvar

instance Functor (UType s) where
  fmap _ = coerce
