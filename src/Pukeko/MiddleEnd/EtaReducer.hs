{-# LANGUAGE ViewPatterns #-}
module Pukeko.MiddleEnd.EtaReducer
  ( reduceModule
  ) where

import Pukeko.Prelude

import           Data.Bitraversable
import qualified Data.Vector        as Vec

import           Pukeko.AST.SystemF
import           Pukeko.AST.Stage   (HasLambda, HasClasses)
import           Pukeko.AST.Type

type ERStage st = (HasLambda st ~ 'False, HasClasses st ~ 'False)

reduceModule :: ERStage st => Module st -> Module st
reduceModule = over (module2decls . traverse . traverse) erDecl

_EVar :: Expr st tv ev -> Maybe ev
_EVar = \case
  EVar x -> Just x
  _      -> Nothing

_TVar :: Type tv -> Maybe tv
_TVar = \case
  TVar v -> Just v
  _      -> Nothing

erDecl :: forall st. ERStage st => Decl st -> Decl st
erDecl top = case top of
  DType{} -> top
  DSign{} -> top
  DPrim{} -> top
  DSupC (MkSupCDecl z vs0 t0 bs0 e0) ->
    case e0 of
      -- Reduce expression parameters:
      EApp (traverse strengthenScope -> Just e1) (traverse _EVar -> Just xs1)
        | length bs0 == length xs1
        , iall (\i -> scope absurd (== i)) xs1 ->
          erDecl (DSupC (MkSupCDecl z vs0 t0 [] (weakenE e1)))
      -- Reduce type paramaters:
      ETyApp
        (bitraverse strengthenScope strengthenScope -> Just e1)
        (traverse _TVar -> Just vs1)
        | null bs0
        , length vs0 == length vs1
        , iall (\i -> scope absurd (== i)) vs1 ->
          DSupC (MkSupCDecl z
            [] (weakenT (mkTUni vs0 t0))
            [] (bimap weakenScope weakenScope e1))
      -- Promote type abstraction when the super combinator doesn't contain any
      -- abstractions itself:
      ETyAbs vs1 e1
        | null vs0 && null bs0
        , TUni (toList -> vs2) t2 <- strengthenT0 t0
        , length vs1 == length vs2 ->
            let e2, e3 :: Expr st (TScope Int Void) (EScope Int Void)
                e2 = first (over _Free strengthenScope0) e1
                -- NOTE: The names of the binders in vs1 and vs2 might differ,
                -- so we need to adjust either t0 or e2.
                e3 = over
                     (flip bitraverse pure . _Bound)
                     (\(i, _) -> (i, (Vec.fromList vs2 Vec.! i)^.qvar2tvar)) e2
            in  erDecl (DSupC (MkSupCDecl z vs2 t2 [] e3))
      ELoc e1 -> erDecl (DSupC (MkSupCDecl z vs0 t0 bs0 (unlctd e1)))
      _ -> top
