{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.Language.TypeChecker.Type
  ( UType (..)
  , UVar (..)
  , (~>)
  , (*~>)
  , appN
  , appTCon
  , open
  , vars
  , subst
  , prettyUVar
  , prettyUType
  )
  where

import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Ratio () -- for precedences in pretty printer
import           Data.STRef
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Pukeko.Error
import           Pukeko.Pretty
import           Pukeko.Language.Type (Type (..))
import qualified Pukeko.Language.Ident as Id

infixr 1 ~>, *~>

data UVar s tv
  = UFree tv Int  -- NOTE: The @Int@ is the level of this unification variable.
  | ULink (UType s tv)

data UType s tv
  = UTVar tv
  | UTArr
  | UTCon Id.TCon
  | UTApp (UType s tv) (UType s tv)
  | UVar (STRef s (UVar s tv))

pattern UTFun :: UType s tv -> UType s tv -> UType s tv
pattern UTFun tx ty = UTApp (UTApp UTArr tx) ty

(~>) :: UType s tv -> UType s tv -> UType s tv
(~>) = UTFun

(*~>) :: [UType s tv] -> UType s tv -> UType s tv
t_args *~> t_res = foldr (~>) t_res t_args

appN :: UType s tv -> [UType s tv] -> UType s tv
appN = foldl UTApp

appTCon :: Id.TCon -> [UType s tv] -> UType s tv
appTCon = appN . UTCon

open :: Type tv -> UType s tv
open = \case
  TVar x     -> UTVar x
  TArr       -> UTArr
  TCon c     -> UTCon c
  TApp tf tp -> UTApp (open tf) (open tp)

vars :: Ord tv => UType s tv -> ST s (Set.Set tv)
vars = \case
  UTVar v -> pure (Set.singleton v)
  UTArr   -> pure Set.empty
  UTCon _ -> pure Set.empty
  UTApp tf tp -> Set.union <$> vars tf <*> vars tp
  UVar uref -> do
    uvar <- readSTRef uref
    case uvar of
      UFree _ _ -> pure Set.empty
      ULink t   -> vars t

subst ::
  forall s tv.
  (Show tv, Ord tv) =>
  Map.Map tv (UType s tv) -> UType s tv -> ST s (UType s tv)
subst env t = runReaderT (subst' t) env
  where
    subst' :: UType s tv
           -> ReaderT (Map.Map tv (UType s tv)) (ST s) (UType s tv)
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

-- * Deep traversals

-- * Pretty printing
prettyUVar :: Pretty tv => PrettyLevel -> Rational -> UVar s tv -> ST s Doc
prettyUVar lvl prec = \case
  UFree x _ -> pure (pretty x)
  ULink t   -> prettyUType lvl prec t

prettyUType :: Pretty tv => PrettyLevel -> Rational -> UType s tv -> ST s Doc
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
