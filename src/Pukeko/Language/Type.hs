{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
module Pukeko.Language.Type
  ( Type (..)
  , UVar (..)
  , Open
  , Closed
  , open
  , var
  , con
  , (~>)
  , (*~>)
  , app
  , appTCon
  , typeInt
  , vars
  , openVars
  , openSubst
  , type2tcon
  , prettyType
  )
  where

import           Control.Lens         (Traversal')
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Ratio () -- for precedences in pretty printer
import           Data.STRef
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Pukeko.Error
import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id

infixr 1 ~>, *~>

data Open s
data Closed

data UVar s
  = Free{_ident :: Id.TVar, _level :: Int}
  | Link{_type  :: Type (Open s)}

data Type a where
  Var  :: Id.TVar          -> Type a
  Arr  ::                     Type a
  Con  :: Id.TCon          -> Type a
  App  :: Type a -> Type a -> Type a
  UVar :: STRef s (UVar s) -> Type (Open s)

pattern Fun :: Type a -> Type a -> Type a
pattern Fun tx ty = App (App Arr tx) ty

var :: Id.TVar -> Type a
var = Var

con :: Id.TCon -> Type a
con = Con

(~>) :: Type a -> Type a -> Type a
(~>) = Fun

(*~>) :: [Type a] -> Type a -> Type a
t_args *~> t_res = foldr (~>) t_res t_args

app :: Type a -> [Type a] -> Type a
app = foldl App

appTCon :: Id.TCon -> [Type a] -> Type a
appTCon = app . Con

typeInt :: Type Closed
typeInt  = appTCon (Id.tcon "Int") []

open :: Type Closed -> Type (Open s)
open = \case
  Var name  -> Var name
  Arr       -> Arr
  Con c     -> Con c
  App tf tp -> App (open tf) (open tp)

vars :: Type Closed -> Set.Set Id.TVar
vars t = runST $ openVars (open t)

openVars :: Type (Open s) -> ST s (Set.Set Id.TVar)
openVars = \case
  Var v -> pure (Set.singleton v)
  Arr   -> pure Set.empty
  Con _ -> pure Set.empty
  App tf tp -> Set.union <$> openVars tf <*> openVars tp
  UVar uref -> do
    uvar <- readSTRef uref
    case uvar of
      Free{}      -> pure Set.empty
      Link{_type} -> openVars _type

openSubst :: Map.Map Id.TVar (Type (Open s)) -> Type (Open s) -> ST s (Type (Open s))
openSubst env t = runReaderT (subst' t) env
  where
    subst' :: Type (Open s)
           -> ReaderT (Map.Map Id.TVar (Type (Open s))) (ST s) (Type (Open s))
    subst' = \case
      Var v -> do
        let e = bug "type instantiation" "unknown variable" (Just $ show v)
        Map.findWithDefault e v <$> ask
      t@Arr     -> pure t
      t@(Con _) -> pure t
      App tf tp -> App <$> subst' tf <*> subst' tp
      t@(UVar uref) -> do
        uvar <- lift $ readSTRef uref
        case uvar of
          Free{}      -> pure t
          Link{_type} -> subst' _type

-- * Deep traversals

-- TODO: Change the order of the parameters of 'Type' and this becomes
-- 'traverse'.
type2tcon :: Traversal' (Type Closed) Id.TCon
type2tcon f = \case
  Var v -> pure (Var v)
  Arr   -> pure Arr
  Con c -> Con <$> f c
  App tf tp -> App <$> type2tcon f tf <*> type2tcon f tp

-- * Pretty printing
prettyUVar :: PrettyLevel -> Rational -> UVar s -> ST s Doc
prettyUVar lvl prec uvar = case uvar of
  Free{_ident} -> return $ pretty _ident
  Link{_type}  -> prettyType lvl prec _type

prettyType :: PrettyLevel -> Rational -> Type (Open s) -> ST s Doc
prettyType lvl prec t = case t of
  Var v -> pure (pretty v)
  Arr   -> pure "(->)"
  Con c -> pure (pretty c)
  Fun tx ty -> do
    px <- prettyType lvl 2 tx
    py <- prettyType lvl 1 ty
    pure $ maybeParens (prec > 1) $ px <+> "->" <+> py
  App tf tx -> do
    pf <- prettyType lvl 3 tf
    px <- prettyType lvl 3 tx
    pure $ maybeParens (prec > 2) $ pf <+> px
  UVar uref -> do
    uvar <- readSTRef uref
    prettyUVar lvl prec uvar


instance Pretty (Type Closed) where
  pPrintPrec lvl prec t = runST $ prettyType lvl prec (open t)

instance Show (Type Closed) where
  show = prettyShow
