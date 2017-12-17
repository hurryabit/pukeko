{-# LANGUAGE DataKinds #-}
module Pukeko.Language.KindChecker.AST
  ( TypeCon
  , ExprCon
  , Module
  , TopLevel (..)
  , Defn
  , Expr
  , Altn
  , Patn
  )
where

import qualified Data.Vector.Sized as Vec

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.TypeResolver.AST as TR
import qualified Pukeko.Language.Ident            as Id
import qualified Pukeko.Language.Type             as Ty

data KINDCHECKER

type TypeCon = TR.TypeCon
type ExprCon = TR.ExprCon

instance Stage KINDCHECKER where
  type ExprConOf KINDCHECKER = ExprCon
  type HasLam    KINDCHECKER = 'True
  type HasMat    KINDCHECKER = 'True

type Module = [TopLevel]

data TopLevel
  =           Val    Pos Id.EVar (Ty.Type TypeCon Ty.Closed)
  | forall n. TopLet Pos (Vec.Vector n (Defn Id.EVar))
  | forall n. TopRec Pos (Vec.Vector n (Defn (FinScope n Id.EVar)))
  |           Asm    Pos Id.EVar String

type Defn = StdDefn KINDCHECKER
type Expr = StdExpr KINDCHECKER
type Altn = StdAltn KINDCHECKER
type Patn = StdPatn KINDCHECKER
