{-# LANGUAGE ConstraintKinds #-}
module Pukeko.Language.DeadCode
  ( cleanModule
  )
where

import           Control.Lens
import qualified Data.Graph    as G
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set

import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std
import           Pukeko.Language.AST.ModuleInfo (info2funs)
import qualified Pukeko.Language.AST.Stage      as St
import qualified Pukeko.Language.Ident          as Id

type ElimStage st = (St.HasTLTyp st ~ 'False, St.HasTLVal st ~ 'False)

cleanModule :: (ElimStage st) => Module st -> Module st
cleanModule (MkModule info0 tops0) =
  let (g, out, in_) = G.graphFromEdges $ map (\t -> (t, topLevelLhs t, deps t)) tops0
      reach = Set.fromList
              $ map (view _2 . out) $ maybe [] (G.reachable g) (in_ Id.main)
      keep = (`Set.member` reach)
      info1 = over info2funs (Map.filterWithKey (const . keep)) info0
      tops1 = filter (keep . topLevelLhs) tops0
  in  MkModule info1 tops1
  where
    deps = Set.toList . Set.setOf (\f -> topLevel2expr (expr2eval f))

topLevelLhs :: (ElimStage st) => TopLevel st -> Id.EVar
topLevelLhs = view lhs . \case
  TLDef (MkDefn b _) -> b
  TLSup b _ _        -> b
  TLAsm b _          -> b

topLevel2expr ::
  (ElimStage st, Applicative f) =>
  (forall tv ev. Expr st tv ev -> f (Expr st tv ev)) -> TopLevel st -> f (TopLevel st)
topLevel2expr f = \case
  TLDef d -> TLDef <$> defn2rhs f d
  TLSup b bs e -> TLSup b bs <$> f e
  TLAsm b s -> pure (TLAsm b s)
