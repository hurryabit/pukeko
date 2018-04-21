{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.Type
  ( IsType (..)
  , NoType (..)
  , GenTypeCstr
  , TypeCstr
  , TypeAtom (..)
  , GenType (..)
  , Type
  , CoercionDir (..)
  , Coercion (..)
  , (:::) (..)

  , pattern TArr
  , pattern TInt
  , pattern TCon
  , pattern TFun
  , pattern TUni'
  , _TApp
  , _TCtx
  , _TUni'

  , closeT
  , (~>)
  , (*~>)
  , mkTApp
  , typeAtomText
  , prettyCstr
  , prettyContext
  , prettyTUni

  , module Pukeko.AST.Unwind
  )
  where

import Pukeko.Prelude
import Pukeko.Pretty

import qualified Bound      as B
import qualified Bound.Name as B
import qualified Bound.Var  as B
import           Control.Lens (Iso', iso)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Deriving
import           Data.Functor.Classes

import           Pukeko.AST.Name
import           Pukeko.AST.Unwind
import           Pukeko.Orphans ()

class IsType t where
  isType :: t -> Maybe Type

data NoType = NoType

type GenTypeCstr v = (Class, GenType v)

type TypeCstr = GenTypeCstr TyVar

data TypeAtom
  = TAArr
  | TAInt
  | TACon TyCon

data GenType tv
  = TVar tv
  | TAtm TypeAtom
  | TApp (GenType tv) (GenType tv)
  | TUni TyVar (B.Scope (B.Name TyVar ()) GenType tv)
  | TCtx (Class, GenType tv) (GenType tv)
    -- NOTE: ^ The first pair is actually a @GenTypeCstr tv@. Unfortunately,
    -- putting the type synonym there breaks @B.makeBound ''GenType@ below.

type Type = GenType TyVar

pattern TArr :: GenType tv
pattern TArr = TAtm TAArr

pattern TInt :: GenType tv
pattern TInt = TAtm TAInt

pattern TCon :: TyCon -> GenType tv
pattern TCon tcon = TAtm (TACon tcon)

pattern TFun :: GenType tv -> GenType tv -> GenType tv
pattern TFun tx ty = TApp (TApp TArr tx) ty

data CoercionDir = Inject | Project

data Coercion = MkCoercion
  { _coeDir  :: CoercionDir
  , _coeTCon :: TyCon
  }

-- | A convenience data type to make pretty printing of (name, type) pairs in
-- the for "name : type" easier.
data a ::: t = a ::: t

makePrisms ''GenType
B.makeBound ''GenType

_Scope1Name :: (Monad f, Eq v) => Iso' (v, B.Scope (B.Name v ()) f v) (v, f v)
_Scope1Name = iso
  (\(v, s) -> (v, B.instantiate1Name (pure v) s))
  (\(v, t) -> (v, B.abstract1Name v t))

pattern TUni' :: TyVar -> Type -> Type
pattern TUni' v t <- TUni v (B.instantiate1Name (TVar v) -> t)
  where TUni' v t = TUni v (B.abstract1Name v t)

_TUni' :: Prism' Type (TyVar, Type)
_TUni' = _TUni . _Scope1Name

closeT :: HasCallStack => Type -> GenType Void
closeT t = fromMaybe (traceJSON t impossible) (B.closed t)

infixr 1 ~>, *~>

(~>) :: GenType tv -> GenType tv -> GenType tv
(~>) = TFun

(*~>) :: Foldable t => t (GenType tv) -> GenType tv -> GenType tv
t_args *~> t_res = foldr (~>) t_res t_args

mkTApp :: (Foldable t) => GenType tv -> t (GenType tv) -> GenType tv
mkTApp = foldl TApp

typeAtomText :: TypeAtom -> Tagged 'TyCon String
typeAtomText = \case
  TAArr   -> "Arr"
  TAInt   -> "Int"
  TACon c -> nameText c

instance IsType NoType where
  isType = const Nothing

instance IsType Type where
  isType = Just

instance Eq1 GenType where
  liftEq eq t1 t2 = case (t1, t2) of
    (TVar x1, TVar x2) -> x1 `eq` x2
    (TAtm a1, TAtm a2) -> a1 == a2
    (TApp tf1 tp1, TApp tf2 tp2) -> liftEq eq tf1 tf2 && liftEq eq tp1 tp2
    (TUni _x1 tq1, TUni _x2 tq2) -> liftEq eq tq1 tq2
    (TCtx (c1, tc1) tq1, TCtx (c2, tc2) tq2) ->
      c1 == c2 && liftEq eq tc1 tc2 && liftEq eq tq1 tq2
    (TVar{}, _) -> False
    (TAtm{}, _) -> False
    (TApp{}, _) -> False
    (TUni{}, _) -> False
    (TCtx{}, _) -> False

instance Eq v => Eq (GenType v) where (==) = eq1

instance Pretty TypeAtom where
  pretty = \case
    TAArr   -> "(->)"
    TAInt   -> "Int"
    TACon c -> pretty c

instance Pretty v => Pretty (GenType v)

instance Pretty v => PrettyPrec (GenType v) where
  prettyPrec = go pretty
    where
      go :: forall v. (v -> Doc) -> Int -> GenType v -> Doc
      go prettyVar prec = \case
        TVar x -> prettyVar x
        TAtm a -> pretty a
        TFun tx ty ->
          maybeParens (prec > 1) (go prettyVar 2 tx <+> "->" <+> go prettyVar 1 ty)
        TApp tf ta ->
          maybeParens (prec > 2) (go prettyVar 2 tf <+> go prettyVar 3 ta)
        t0@TUni{} ->
          goUni [] prettyVar t0
          where
            goUni :: forall v. [TyVar] -> (v -> Doc) -> GenType v -> Doc
            goUni vs prettyVar = \case
              TUni v t1 ->
                goUni (v:vs) (B.unvar (pretty . B.name) prettyVar) (B.fromScope t1)
              t1 ->
                prettyTUni prec (reverse vs) (go prettyVar 0 t1)
        t0@TCtx{} ->
          let (cstrs0, t1) = unwindr _TCtx t0
              cstrs1 = [ pretty clss <+> go prettyVar 3 typ | (clss, typ) <- cstrs0 ]
          in  maybeParens (prec > 0)
              (parens (hsep (punctuate "," cstrs1)) <+> "=>" <+> go prettyVar 0 t1)

prettyCstr :: TypeCstr -> Doc
prettyCstr (clss, typ) = pretty clss <+> prettyPrec 3 typ

prettyContext :: [TypeCstr] -> Doc
prettyContext cstrs
  | null cstrs = mempty
  | otherwise  = parens (hsep (punctuate "," (map prettyCstr cstrs))) <+> "=>"

prettyTUni :: Foldable t => Int -> t TyVar -> Doc -> Doc
prettyTUni prec vs tq =
  maybeParens (prec > 0) ("forall" <+> hsepMap pretty vs <> "." <+> tq)

instance (Pretty a, Pretty t) => Pretty (a ::: t) where
instance (Pretty a, Pretty t) => PrettyPrec (a ::: t) where
  prettyPrec prec (x ::: t) = maybeParens (prec > 0) (pretty x <+> "::" <+> pretty t)

deriving instance Functor     GenType
deriving instance Foldable    GenType
deriving instance Traversable GenType

deriving instance Show TypeAtom
deriving instance Show CoercionDir
deriving instance Show Coercion

deriveShow1 ''GenType
instance Show tv => Show (GenType tv) where showsPrec = showsPrec1

deriving instance Eq  TypeAtom
deriving instance Ord TypeAtom

deriveToJSON defaultOptions ''TypeAtom
deriveToJSON defaultOptions ''CoercionDir
deriveToJSON defaultOptions ''Coercion
deriveToJSON1 defaultOptions ''GenType

instance ToJSON v => ToJSON (GenType v) where
  toJSON = liftToJSON toJSON toJSONList
