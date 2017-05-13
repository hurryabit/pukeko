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
  , (~>)
  , (*~>)
  , app
  , typeInt
  , vars
  , prettyType
  )
  where

import Control.Monad.ST
import Data.Ratio () -- for precedences in pretty printer
import Data.Set (Set)
import Data.STRef

import qualified Data.Set as Set

import Pukeko.Pretty hiding (int)

import qualified Pukeko.Language.Ident as Ident

infixr 1 ~>, *~>

data ADT con = MkADT
  { _name         :: Ident.Con
  , _params       :: [Ident.TVar]
  , _constructors :: [Constructor con]
  }

mkADT :: Ident.Con -> con -> [Ident.TVar] -> [Constructor con] -> ADT con
mkADT _name con _params constructors = MkADT
  { _name
  , _params
  , _constructors = zipWith (\_tag constr -> constr { _adt = con, _tag }) [0..] constructors
  }

data Constructor con = MkConstructor
  { _adt    :: con
  , _name   :: Ident.Con
  , _tag    :: Int
  , _fields :: [Type con Closed]
  }

mkConstructor :: Ident.Con -> [Type con Closed] -> Constructor con
mkConstructor _name _fields =
  MkConstructor { _adt = undefined, _name, _tag = undefined, _fields }

adt :: con -> [Type con Closed] -> Type con Closed
adt adt = app adt

-- constructors :: ADT con -> [(Ident.Con, Type con Closed)]
-- constructors t@MkADT{ _params, _constructors } = map f _constructors
--   where
--     f MkConstructor{ _name, _fields } =
--       (_name, foldr (~>) (adt t $ map var _params) _fields)

typeOf :: Constructor (ADT Ident.Con) -> Type (ADT Ident.Con) Closed
typeOf MkConstructor{ _adt, _fields } =
  foldr (~>) (adt _adt $ map var $ _params _adt) _fields


data Open s
data Closed

data UVar con s
  = Free { _ident :: Ident.TVar, _level :: Int }
  | Link { _type  :: Type con (Open s) }

data Type con a where
  TVar :: Ident.TVar                 -> Type con a
  TFun :: Type con a ->  Type con a  -> Type con a
  TApp :: con        -> [Type con a] -> Type con a
  UVar :: STRef s (UVar con s)       -> Type con (Open s)

var :: Ident.TVar -> Type con a
var = TVar

(~>) :: Type con a -> Type con a -> Type con a
(~>) = TFun

(*~>) :: [Type con a] -> Type con a -> Type con a
t_args *~> t_res = foldr (~>) t_res t_args

app :: con -> [Type con a] -> Type con a
app = TApp

-- TODO: Remove this undefined hack.
typeInt :: Type (ADT Ident.Con) Closed
typeInt  = app (mkADT (Ident.constructor "Int") undefined [] []) []

open :: Type con Closed -> Type con (Open s)
open t =
  case t of
    TVar name  -> TVar name
    TFun tx ty -> TFun (open tx) (open ty)
    TApp c  ts -> TApp c (map open ts)

vars :: Type con Closed -> Set Ident.TVar
vars t = case t of
  TVar _ident -> Set.singleton _ident
  TFun t_arg t_res -> vars t_arg `Set.union` vars t_res
  TApp _ t_params -> Set.unions (map vars t_params)

prettyUVar :: Pretty con => PrettyLevel -> Rational -> UVar con s -> ST s Doc
prettyUVar lvl prec uvar = case uvar of
  Free{_ident} -> return $ pretty _ident
  Link{_type}  ->  prettyType lvl prec _type

prettyType :: Pretty con => PrettyLevel -> Rational -> Type con (Open s) -> ST s Doc
prettyType lvl prec t =
  case t of
    TVar v -> return $ pretty v
    TFun tx ty -> do
      px <- prettyType lvl 2 tx
      py <- prettyType lvl 1 ty
      return $ maybeParens (prec > 1) $ px <+> "->" <+> py
    TApp c [] -> return $ pretty c
    TApp c ts -> do
      ps <- mapM (prettyType lvl 3) ts
      return $ maybeParens (prec > 2) $ pretty c <+> hsep ps
    UVar uref -> do
      uvar <- readSTRef uref
      prettyUVar lvl prec uvar

instance Pretty (ADT Ident.Con) where
  pPrintPrec lvl prec MkADT{_name} = pPrintPrec lvl prec _name

instance Pretty (Constructor (ADT Ident.Con)) where
  pPrintPrec lvl prec MkConstructor{_name} = pPrintPrec lvl prec _name

instance Pretty con => Pretty (Type con Closed) where
  pPrintPrec lvl prec t = runST $ prettyType lvl prec (open t)

instance Pretty con => Show (Type con Closed) where
  show = prettyShow
