{-# LANGUAGE ConstraintKinds #-}
module Pukeko.Language.TypeEraser
  ( eraseModule
  ) where

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage as St
import           Pukeko.Language.Type      (NoType (..))

type Erasable st1 st2 =
  ( St.SameTopNodes st1 st2
  , St.HasLambda st1 ~ 'True
  , St.HasLambda st2 ~ 'True
  , St.HasNested st1 ~ St.HasNested st2
  , St.HasTypes  st2 ~ 'False
  , St.StageType st2 ~ NoType
  , St.SameModuleInfo st1 st2
  )

eraseModule :: (Erasable st1 st2) => Module st1 -> Module st2
eraseModule (MkModule info tops) = MkModule info (map eraseTopLevel tops)

eraseTopLevel :: (Erasable st1 st2) => TopLevel st1 -> TopLevel st2
eraseTopLevel = \case
  TLTyp w ds -> TLTyp w ds
  TLVal w x t -> TLVal w x t
  TLDef   d -> TLDef (eraseDefn d)
  -- TLSup w bs e -> TLSup w (fmap eraseBind bs) (eraseExpr e)
  TLAsm b s -> TLAsm (eraseBind b) s

eraseDefn :: (Erasable st1 st2) => Defn st1 tv ev -> Defn st2 Void ev
eraseDefn (MkDefn b e) = MkDefn (eraseBind b) (eraseExpr e)

eraseExpr :: (Erasable st1 st2) => Expr st1 tv ev -> Expr st2 Void ev
eraseExpr = \case
  EVar w x      -> EVar w x
  EVal w z      -> EVal w z
  ECon w c      -> ECon w c
  ENum w n      -> ENum w n
  EApp w e0 es  -> EApp w (eraseExpr e0) (map eraseExpr es)
  ELam w bs e0 _t -> ELam w (fmap eraseBind bs) (eraseExpr e0) NoType
  ELet w ds e0  -> ELet w (fmap eraseDefn ds) (eraseExpr e0)
  ERec w ds e0  -> ERec w (fmap eraseDefn ds) (eraseExpr e0)
  ECas w e0 cs  -> ECas w (eraseExpr e0) (fmap eraseCase cs)
  EMat w e0 as  -> EMat w (eraseExpr e0) (fmap eraseAltn as)
  ETyAbs _ _ e0 -> eraseExpr e0
  ETyApp _ e0 _ -> eraseExpr e0

eraseBind :: (Erasable st1 st2) => Bind st1 tv -> Bind st2 Void
eraseBind (MkBind w x _) = MkBind w x NoType

eraseCase :: (Erasable st1 st2) => Case st1 tv ev -> Case st2 Void ev
eraseCase (MkCase w c _ts bs e) = MkCase w c [] bs (eraseExpr e)

eraseAltn :: (Erasable st1 st2) => Altn st1 tv ev -> Altn st2 Void ev
eraseAltn (MkAltn w p e) = MkAltn w (erasePatn p) (eraseExpr e)

erasePatn :: (Erasable st1 st2) => Patn st1 tv -> Patn st2 Void
erasePatn = \case
  PWld w          -> PWld w
  PVar w x        -> PVar w x
  PCon w c _ts ps -> PCon w c [] (map erasePatn ps)
