module Pukeko.MiddleEnd.EtaReducer
  ( reduceModule
  ) where

import Pukeko.Prelude

import qualified Data.Set     as Set

import           Pukeko.AST.Expr.Optics
import           Pukeko.AST.Name
import           Pukeko.AST.SuperCore
import           Pukeko.AST.Type

reduceModule :: Module -> Module
reduceModule = over (mod2supcs . traverse) erSupCDecl

type FreeVars = (Set TyVar, Set TmVar)

freeVars :: Arg -> FreeVars
freeVars = \case
  TmArg e -> (setOf (expr2type . traverse) e, setOf freeTmVar e)
  TyArg t -> (setOf traverse t, Set.empty)

-- | Takes a list of abstractions and applications (together with the free
-- variables of the function of that application) in reverse order, i.e., such
-- that eta reduction would start at the head, and applies eta reduction as long
-- as possible.
etaReduce :: ([Par], [(Arg, FreeVars)]) -> ([Par], [Arg])
etaReduce = \case
  (TmPar (x, _):ps, (TmArg (EVar y), fv):as)
    | x == y && x `Set.notMember` snd fv -> etaReduce (ps, as)
  (TyPar  v    :ps, (TyArg (TVar w), fv):as)
    | v == w && v `Set.notMember` fst fv -> etaReduce (ps, as)
  (ps, as) -> (ps, map fst as)

erSupCDecl :: FuncDecl (Only SupC) -> FuncDecl (Only SupC)
erSupCDecl (SupCDecl z t ps0 e0) = SupCDecl z t (reverse ps1) e2
  where
    (e1, as0) = unwindl _EApp e0
    fvs = scanl (<>) (freeVars (TmArg e1)) (map freeVars as0)
    (ps1, as1) = etaReduce (reverse ps0, reverse (zip as0 fvs))
    e2 = rewindl EApp e1 (reverse as1)
