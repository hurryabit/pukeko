{-# LANGUAGE DataKinds #-}
module Pukeko.Language.KindChecker.AST
  ( ModuleInfo
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
import qualified Pukeko.Language.Ident            as Id
import qualified Pukeko.Language.Type             as Ty

data KINDCHECKER

instance Stage KINDCHECKER where
  type StageId     KINDCHECKER = 300
  type StdTopLevel KINDCHECKER = TopLevel

data TopLevel
  =           Val    Pos Id.EVar (Ty.Type Ty.Closed)
  | forall n. TopLet Pos (Vec.Vector n (Defn Id.EVar))
  | forall n. TopRec Pos (Vec.Vector n (Defn (FinScope n Id.EVar)))
  |           Asm    Pos Id.EVar String

type ModuleInfo = GenModuleInfo 'True
type Module = StdModule KINDCHECKER
type Defn = StdDefn KINDCHECKER
type Expr = StdExpr KINDCHECKER
type Altn = StdAltn KINDCHECKER
type Patn = StdPatn KINDCHECKER
