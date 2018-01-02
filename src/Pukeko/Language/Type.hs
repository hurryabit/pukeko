{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
module Pukeko.Language.Type
  ( ADT (..)
  , Constructor (..)
  -- , constructors
  , mkADT
  , mkConstructor
  , typeOf
  , Type (..)
  , UVar (..)
  , Open
  , Closed
  , open
  , var
  , con
  , (~>)
  , (*~>)
  , app
  , appADT
  , typeInt
  , vars
  , openVars
  , openSubst
  , type2con
  , prettyType
  )
  where

import           Control.Lens         (Traversal)
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Ratio () -- for precedences in pretty printer
import           Data.STRef
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Pukeko.Error
import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Ident

infixr 1 ~>, *~>

data ADT tcon = MkADT
  { _name         :: Ident.TCon
  , _params       :: [Ident.TVar]
  , _constructors :: [Constructor tcon]
  }

mkADT :: Ident.TCon -> tcon -> [Ident.TVar] -> [Constructor tcon] -> ADT tcon
mkADT _name tcon _params constructors = MkADT
  { _name
  , _params
  , _constructors = zipWith (\_tag constr -> constr { _adt = tcon, _tag }) [0..] constructors
  }

data Constructor tcon = MkConstructor
  { _adt    :: tcon
  , _name   :: Ident.DCon
  , _tag    :: Int
  , _fields :: [Type tcon Closed]
  }

mkConstructor :: Ident.DCon -> [Type tcon Closed] -> Constructor tcon
mkConstructor _name _fields =
  MkConstructor { _adt = undefined, _name, _tag = undefined, _fields }

typeOf :: Constructor (ADT Ident.TCon) -> Type (ADT Ident.TCon) Closed
typeOf MkConstructor{ _adt, _fields } =
  foldr (~>) (appADT _adt $ map var $ _params _adt) _fields


data Open s
data Closed

data UVar tcon s
  = Free{_ident :: Ident.TVar, _level :: Int}
  | Link{_type  :: Type tcon (Open s)}

data Type tcon a where
  Var  :: Ident.TVar                 -> Type tcon a
  Arr  ::                               Type tcon a
  Con  :: tcon                       -> Type tcon a
  App  :: Type tcon a -> Type tcon a -> Type tcon a
  UVar :: STRef s (UVar tcon s)      -> Type tcon (Open s)

pattern Fun :: Type tcon a -> Type tcon a -> Type tcon a
pattern Fun tx ty = App (App Arr tx) ty

var :: Ident.TVar -> Type tcon a
var = Var

con :: tcon -> Type tcon a
con = Con

(~>) :: Type tcon a -> Type tcon a -> Type tcon a
(~>) = Fun

(*~>) :: [Type tcon a] -> Type tcon a -> Type tcon a
t_args *~> t_res = foldr (~>) t_res t_args

app :: Type tcon a -> [Type tcon a] -> Type tcon a
app = foldl App

appADT :: tcon -> [Type tcon a] -> Type tcon a
appADT = app . Con

-- TODO: Remove this undefined hack.
typeInt :: Type (ADT Ident.TCon) Closed
typeInt  = appADT (mkADT (Ident.tcon "Int") undefined [] []) []

open :: Type tcon Closed -> Type tcon (Open s)
open = \case
  Var name  -> Var name
  Arr       -> Arr
  Con c     -> Con c
  App tf tp -> App (open tf) (open tp)

vars :: Type tcon Closed -> Set.Set Ident.TVar
vars t = runST $ openVars (open t)

openVars :: Type tcon (Open s) -> ST s (Set.Set Ident.TVar)
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

openSubst :: Map.Map Ident.TVar (Type tcon (Open s)) -> Type tcon (Open s) -> ST s (Type tcon (Open s))
openSubst env t = runReaderT (subst' t) env
  where
    subst' :: Type tcon (Open s)
           -> ReaderT (Map.Map Ident.TVar (Type tcon (Open s))) (ST s) (Type tcon (Open s))
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
type2con :: Traversal (Type con1 Closed) (Type con2 Closed) con1 con2
type2con f = \case
  Var v -> pure (Var v)
  Arr   -> pure Arr
  Con c -> Con <$> f c
  App tf tp -> App <$> type2con f tf <*> type2con f tp

-- * Pretty printing
prettyUVar :: Pretty tcon => PrettyLevel -> Rational -> UVar tcon s -> ST s Doc
prettyUVar lvl prec uvar = case uvar of
  Free{_ident} -> return $ pretty _ident
  Link{_type}  -> prettyType lvl prec _type

prettyType :: Pretty tcon => PrettyLevel -> Rational -> Type tcon (Open s) -> ST s Doc
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


instance Pretty (ADT Ident.TCon) where
  pPrintPrec lvl prec MkADT{_name} = pPrintPrec lvl prec _name

instance Pretty (Constructor (ADT Ident.TCon)) where
  pPrintPrec lvl prec MkConstructor{_name} = pPrintPrec lvl prec _name

instance Pretty tcon => Pretty (Type tcon Closed) where
  pPrintPrec lvl prec t = runST $ prettyType lvl prec (open t)

instance Pretty tcon => Show (Type tcon Closed) where
  show = prettyShow
