{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.Expr
  ( Expr (..)
  , BindMode (..)
  , Atom (..)
  , TmBinder
  , Bind (..)
  , Arg  (..)
  , Par  (..)
  , Altn (..)
  , Patn (..)

  , pattern EVal
  , pattern ECon
  , pattern ENum
  , pattern ETmApp
  , pattern ETyApp
  , pattern EDxApp
  , pattern ETmAbs
  , pattern ETyAbs
  , pattern EDxAbs

  , altn2patn
  , altn2expr

  , _TmArg
  , _TyArg
  , _TmPar
  , _TyPar
  , _AVal
  , _EAtm
  , _EApp
  , _ETmApp
  , _ETyApp
  , _EAbs
  , _ETyAbs
  -- , _EDxAbs

  , mkETmAbs
  , unwindEAbs
  , mkTAbs
  , unEAnn

  , prettyEAbs
  )
  where

import Pukeko.Prelude

import Control.Lens (makeLensesFor, prism')
import Data.Aeson
import Data.Aeson.TH
import Unsafe.Coerce

import           Pukeko.AST.Dict
import           Pukeko.AST.Language
import           Pukeko.AST.Name
import qualified Pukeko.AST.Operator as Op
import           Pukeko.AST.Type
import           Pukeko.Pretty

data Atom
  = AVal TmVar
  | ACon TmCon
  | ANum Int

type TmBinder ty = (TmVar, ty)

data Arg lg
  =                TmArg (Expr lg)
  | HasTyApp lg => TyArg (TypeOf lg)
  | HasDxApp lg => DxArg (DictOf lg)

data Par lg
  = HasTmAbs lg => TmPar (TmBinder (TypeOf lg))
  | HasTyAbs lg => TyPar TyVar
  | HasDxAbs lg => DxPar (DxBinder (TypeOf lg))

data BindMode = BindPar | BindRec

data Bind lg
  = TmNonRec (TmBinder (TypeOf lg)) (Expr lg)
  | TmRec [(TmBinder (TypeOf lg),  Expr lg)]

data Patn lg
  = IsNested lg ~ True  => PWld
  | IsNested lg ~ True  => PVar    (TmBinder (TypeOf lg))
  | IsNested lg ~ True  => PCon    TmCon [Patn lg]
  | IsNested lg ~ False => PSimple TmCon [Maybe (TmBinder (TypeOf lg))]

data Altn lg = MkAltn
  { _altn2patn :: Patn lg
  , _altn2expr :: Expr lg
  }

data Expr lg
  = IsLambda lg ~ True   => ELoc (Lctd (Expr lg))
  |                         EVar TmVar
  |                         EAtm Atom
  |                         EApp (Expr lg) (Arg  lg)
  | IsLambda lg ~ True   => EAbs (Par  lg) (Expr lg)
  |                         ELet (Bind lg) (Expr lg)
  |                         EMat (TypeOf lg) (Expr lg) (NonEmpty (Altn lg))
  |                         ECast (Coercion, TypeOf lg) (Expr lg)
  | (IsLambda lg ~ True, IsPreTyped lg ~ True) => ETyAnn (TypeOf lg) (Expr lg )

pattern EVal :: TmVar -> Expr lg
pattern EVal z = EAtm (AVal z)

pattern ECon :: TmCon -> Expr lg
pattern ECon c = EAtm (ACon c)

pattern ENum :: Int -> Expr lg
pattern ENum n = EAtm (ANum n)

pattern ETmApp :: Expr lg -> Expr lg -> Expr lg
pattern ETmApp fun arg = EApp fun (TmArg arg)

pattern ETyApp :: HasTyApp lg => Expr lg -> TypeOf lg -> Expr lg
pattern ETyApp fun typ = EApp fun (TyArg typ)

pattern EDxApp :: HasDxApp lg => Expr lg -> DictOf lg -> Expr lg
pattern EDxApp fun dict = EApp fun (DxArg dict)

pattern ETmAbs :: (IsLambda lg ~ True, HasTmAbs lg) =>
  TmBinder (TypeOf lg) -> Expr lg -> Expr lg
pattern ETmAbs par body = EAbs (TmPar par) body

pattern ETyAbs :: (IsLambda lg ~ True, HasTyAbs lg) => TyVar -> Expr lg -> Expr lg
pattern ETyAbs par body = EAbs (TyPar par) body

pattern EDxAbs :: (IsLambda lg ~ True, HasDxAbs lg) =>
  DxBinder (TypeOf lg) -> Expr lg -> Expr lg
pattern EDxAbs dict body = EAbs (DxPar dict) body

-- * Derived optics
makePrisms ''Atom
makePrisms ''Expr
makePrisms ''Arg
makePrisms ''Par
makeLensesFor [("_altn2patn", "altn2patn")]''Altn

altn2expr :: (TypeOf lg1 ~ TypeOf lg2, IsNested lg1 ~ IsNested lg2) =>
  Lens (Altn lg1) (Altn lg2) (Expr lg1) (Expr lg2)
-- FIXME: This 'unsafeCoerce' must go!
altn2expr f (MkAltn p e) = MkAltn (unsafeCoerce p) <$> f e

_ETmApp :: Prism' (Expr lg) (Expr lg, Expr lg)
_ETmApp = prism' (uncurry ETmApp) $ \case
  ETmApp e a -> Just (e, a)
  _          -> Nothing

_ETyApp :: HasTyApp lg => Prism' (Expr lg) (Expr lg, TypeOf lg)
_ETyApp = prism' (uncurry ETyApp) $ \case
  ETyApp e t -> Just (e, t)
  _          -> Nothing

_ETyAbs :: (IsLambda lg ~ True, HasTyAbs lg) => Prism' (Expr lg) (TyVar, Expr lg)
_ETyAbs = prism' (uncurry ETyAbs) $ \case
  ETyAbs v e -> Just (v, e)
  _          -> Nothing

-- * Smart constructors

mkETmAbs ::
  (IsLambda lg ~ True, IsPreTyped lg ~ True, TypeOf lg ~ Type) =>
  [TmBinder (TypeOf lg)] -> Type -> Expr lg -> Expr lg
mkETmAbs bs0 t0 e0 = case bs0 of
  [] -> e0
  _  -> rewindr ETmAbs bs0 (ETyAnn t0 e0)

unwindEAbs :: forall lg. (IsLambda lg ~ True) => Expr lg -> ([Par lg], Expr lg)
unwindEAbs = go []
  where
    go :: [Par lg] -> Expr lg -> ([Par lg], Expr lg)
    go params = \case
      EAbs param body -> go (param:params) body
      -- NOTE: We do this only conditional on the inner expression being a
      -- lambda to not strip the very last type annotation in a chain of
      -- lambdas.
      ETyAnn _ expr@EAbs{} -> go params expr
      body -> (reverse params, body)

mkTAbs :: TypeOf lg ~ Type => Par lg -> Type -> Type
mkTAbs = \case
  TmPar (_, t) -> TFun t
  TyPar v      -> TUni' v
  DxPar (_, c) -> TCtx c

unEAnn :: IsLambda lg ~ True => Expr lg -> Expr lg
unEAnn = \case
  ETyAnn _ e -> unEAnn e
  e          -> e

-- * Pretty printing

instance IsTyped lg => Pretty (Bind lg) where
  pretty = \case
    TmNonRec b e -> "let" <+> prettyBind (b, e)
    TmRec [] -> impossible
    TmRec (b0:bs) ->
      "let rec" <+> prettyBind b0 $$ vcatMap (\b -> "and" <+> prettyBind b) bs
    where
      prettyBind ((x, t), e) = hang (pretty (x ::: t) <+> "=") 2 (pretty e)

instance Pretty Atom where
  pretty = \case
    AVal z -> pretty z
    ACon c -> pretty c
    ANum n -> pretty n

prettyTyArg :: Type -> Doc
prettyTyArg t = "@" <> prettyPrec 3 t

instance IsTyped lg => Pretty (Arg lg) where
  pretty = \case
    TmArg e -> prettyPrec (succ Op.aprec) e
    TyArg t -> prettyTyArg t
    DxArg d -> braces (pretty d)

instance TypeOf lg ~ Type => Pretty (Par lg) where
  pretty = \case
    TmPar (x, t) -> parens (pretty (x ::: t))
    TyPar v      -> "@" <> pretty v
    DxPar (_, c) -> braces (prettyCstr c)

instance IsTyped lg => Pretty (Expr lg)

instance IsTyped lg => PrettyPrec (Expr lg) where
  prettyPrec prec e0 = case e0 of
    ELoc l -> prettyPrec prec l
    EVar x -> pretty x
    EAtm a -> pretty a
    EApp{} ->
      let (e1, as) = unwindl _EApp e0
      in  maybeParens (prec > Op.aprec) $ prettyPrec Op.aprec e1 <+> hsepMap pretty as
    EAbs{} ->
      let (ps, e1) = unwindr _EAbs e0
      in  prettyEAbs prec ps e1
    ELet bs e -> maybeParens (prec > 0) (sep [pretty bs, "in"] $$ pretty e)
    -- FIXME: Collect lambdas.
    EMat _ e as ->
      maybeParens (prec > 0) $
        "case" <+> pretty e <+> "of"
        $$ vcatMap pretty as
    ECast (MkCoercion dir tcon, _typ) e0 ->
      maybeParens (prec > Op.aprec) $
        "coerce" <+> "@" <> parens (d_from <+> "->" <+> d_to)
        <+> prettyPrec (Op.aprec+1) e0
      where
        (d_from, d_to) = case dir of
          Inject  -> ("_", pretty tcon)
          Project -> (pretty tcon, "_")
    -- TODO: Decide if it's a good idea to swallow type annotations. Probably,
    -- make a distinction between those given by the user and those generated
    -- during type inference.
    ETyAnn _ e -> prettyPrec prec e

prettyEAbs :: (IsTyped lg1, IsTyped lg2) => Int -> [Par lg1] -> Expr lg2 -> Doc
prettyEAbs prec pars body
  | null pars = prettyPrec prec body
  | otherwise =
      maybeParens (prec > 0) $
      hang ("\\" <> hsepMap pretty pars <+> "->") 2 (pretty body)

instance IsTyped lg => Pretty (Altn lg) where
  pretty (MkAltn p t) = hang ("|" <+> pretty p <+> "->") 2 (pretty t)

instance TypeOf lg ~ Type => Pretty (Patn lg)

-- TODO: Print types of binders as well.
instance TypeOf lg ~ Type => PrettyPrec (Patn lg) where
  prettyPrec prec = \case
    PWld -> "_"
    PVar x    -> pretty (fst x)
    PCon c ps ->
      maybeParens (prec > 0 && not (null ps)) $ pretty c <+> hsepMap (prettyPrec 1) ps
    PSimple c bs ->
      maybeParens (prec > 0 && not (null bs))
      $ pretty c <+> hsepMap (maybe "_" (pretty . fst)) bs

deriving instance                     Show  BindMode
deriving instance IsTyped lg => Show (Bind lg)
deriving instance IsTyped lg => Show (Arg  lg)
deriving instance IsTyped lg => Show (Par  lg)
deriving instance IsTyped lg => Show (Expr lg)
deriving instance IsTyped lg => Show (Altn lg)
deriving instance                     Show  Atom
deriving instance IsTyped lg => Show (Patn lg)

deriveToJSON defaultOptions ''BindMode
deriveToJSON defaultOptions ''Atom

instance IsTyped lg => ToJSON (Patn lg) where
  toJSON = $(mkToJSON defaultOptions ''Patn)
instance IsTyped lg => ToJSON (Altn lg) where
  toJSON = $(mkToJSON defaultOptions ''Altn)
instance IsTyped lg => ToJSON (Bind lg) where
  toJSON = $(mkToJSON defaultOptions ''Bind)
instance IsTyped lg => ToJSON (Arg  lg) where
  toJSON = $(mkToJSON defaultOptions ''Arg)
instance IsTyped lg => ToJSON (Par  lg) where
  toJSON = $(mkToJSON defaultOptions ''Par)
instance IsTyped lg => ToJSON (Expr lg) where
  toJSON = $(mkToJSON defaultOptions ''Expr)
