{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.Language.TypeChecker.UType
  ( UType (..)
  , UVar (..)
  , UTypeSchema (..)
  , (~>)
  , (*~>)
  , appN
  , appTCon
  , open
  , openSchema
  , subst
  , prettyUVar
  , prettyUType
  )
  where

import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Foldable     (toList)
import           Data.STRef
import qualified Data.Map          as Map
import qualified Data.Vector.Sized as Vec

import           Pukeko.Error
import           Pukeko.Pretty
import           Pukeko.Language.Type (Type (..), TypeSchema (..))
import qualified Pukeko.Language.Ident as Id
import           Pukeko.Language.AST.Scope

infixr 1 ~>, *~>

data UVar s
  = UFree Id.TVar Int  -- NOTE: The @Int@ is the level of this unification variable.
  | ULink (UType s)

data UType s
  = UTVar Id.TVar
  | UTArr
  | UTCon Id.TCon
  | UTApp (UType s) (UType s)
  | UVar (STRef s (UVar s))

data UTypeSchema s = MkUTypeSchema [Id.TVar] (UType s)

pattern UTFun :: UType s -> UType s -> UType s
pattern UTFun tx ty = UTApp (UTApp UTArr tx) ty

(~>) :: UType s -> UType s -> UType s
(~>) = UTFun

(*~>) :: [UType s] -> UType s -> UType s
t_args *~> t_res = foldr (~>) t_res t_args

appN :: UType s -> [UType s] -> UType s
appN = foldl UTApp

appTCon :: Id.TCon -> [UType s] -> UType s
appTCon = appN . UTCon

open :: Type Id.TVar -> UType s
open = \case
  TVar x     -> UTVar x
  TArr       -> UTArr
  TCon c     -> UTCon c
  TApp tf tp -> UTApp (open tf) (open tp)

openSchema :: TypeSchema -> UTypeSchema s
openSchema (MkTypeSchema xs t0) =
  let t1 = fmap (scope (xs Vec.!) absurd) t0
  in  MkUTypeSchema (toList xs) (open t1)

subst :: Map.Map Id.TVar (UType s) -> UType s -> ST s (UType s)
subst env t = runReaderT (subst' t) env
  where
    subst' :: UType s
           -> ReaderT (Map.Map Id.TVar (UType s)) (ST s) (UType s)
    subst' = \case
      UTVar x -> do
        let e = bug "type instantiation" "unknown variable" (Just $ show x)
        Map.findWithDefault e x <$> ask
      t@UTArr     -> pure t
      t@(UTCon _) -> pure t
      UTApp tf tp -> UTApp <$> subst' tf <*> subst' tp
      t0@(UVar uref) -> do
        uvar <- lift $ readSTRef uref
        case uvar of
          UFree _ _ -> pure t0
          ULink t1  -> subst' t1

-- * Pretty printing
prettyUVar :: PrettyLevel -> Rational -> UVar s -> ST s Doc
prettyUVar lvl prec = \case
  UFree x _ -> pure (pretty x)
  ULink t   -> prettyUType lvl prec t

prettyUType :: PrettyLevel -> Rational -> UType s -> ST s Doc
prettyUType lvl prec = \case
  UTVar v -> pure (pretty v)
  UTArr   -> pure "(->)"
  UTCon c -> pure (pretty c)
  UTFun tx ty -> do
    px <- prettyUType lvl 2 tx
    py <- prettyUType lvl 1 ty
    pure $ maybeParens (prec > 1) $ px <+> "->" <+> py
  UTApp tf tx -> do
    pf <- prettyUType lvl 3 tf
    px <- prettyUType lvl 3 tx
    pure $ maybeParens (prec > 2) $ pf <+> px
  UVar uref -> do
    uvar <- readSTRef uref
    prettyUVar lvl prec uvar
