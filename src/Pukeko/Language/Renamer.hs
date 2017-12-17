{-# LANGUAGE TupleSections #-}
-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.Language.Renamer
  ( Rn.Module
  , renameModule
  )
  where

import           Control.Lens
import qualified Data.Map          as Map
import qualified Data.Vector.Sized as Vec

import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.Parser.AST     as Ps
import qualified Pukeko.Language.Renamer.AST    as Rn
import qualified Pukeko.Language.Ident          as Id

renameModule :: Ps.Module -> Rn.Module
renameModule = map ixTopLevel

ixTopLevel :: Ps.TopLevel -> Rn.TopLevel
ixTopLevel top = case top of
  Ps.TypDef p ts  -> Rn.TypDef p ts
  Ps.Val    p x t -> Rn.Val    p x t
  Ps.TopLet p ds  -> ixDefns ds $ \ds1 _   -> Rn.TopLet p (fmap (rhs2 %~ ixExpr) ds1)
  Ps.TopRec p ds  -> ixDefns ds $ \ds1 abs -> Rn.TopRec p (fmap (rhs2 %~ abs)    ds1)
  Ps.Asm    p x a -> Rn.Asm    p x a

ixExpr :: Ps.Expr Id.EVar -> Rn.Expr Id.EVar
ixExpr expr = case expr of
  Ps.Var p x      -> Var p x
  Ps.Con p c      -> Con p c
  Ps.Num p n      -> Num p n
  Ps.App p t  us  -> App p (ixExpr t) (map ixExpr us)
  -- Ps.If  p t  u v -> If  p (ixExpr t) (ixExpr u) (ixExpr v)
  Ps.Mat p ts as  -> Mat p (ixExpr ts) (map ixAltn as)
  Ps.Lam p bs t   -> ixWithBinds (Lam p) bs t
  Ps.Let w ds t -> ixDefns ds $ \ds1 abs -> Let w (fmap (rhs2 %~ ixExpr) ds1) (abs t)
  Ps.Rec w ds t -> ixDefns ds $ \ds1 abs -> Rec w (fmap (rhs2 %~ abs)    ds1) (abs t)

ixExprAbs :: Map.Map Id.EVar i -> Ps.Expr Id.EVar -> Rn.Expr (Scope i Id.EVar)
ixExprAbs mp =
  let f x = fmap (,x) (x `Map.lookup` mp)
  in  abstract f . ixExpr

ixWithBinds
  :: (forall n. Vec.Vector n Bind -> Rn.Expr (FinScope n Id.EVar) -> a)
  -> [Bind]
  -> Ps.Expr Id.EVar
  -> a
ixWithBinds mk bs0 t = Vec.withList bs0 $ \bs1 ->
  let mp = ifoldMap (\i -> maybe mempty (\x -> Map.singleton x i) . bindName) bs1
  in  mk bs1 (ixExprAbs mp t)

ixWithPatn ::
  (Rn.Patn -> Rn.Expr (Scope Id.EVar Id.EVar) -> a) ->
  Ps.Patn                                      ->
  Ps.Expr Id.EVar                              ->
  a
ixWithPatn mk p t =
  let mp = Map.fromList $ map (\x -> (x, x)) $ toListOf (patn2bind . _Name . _2) p
  in  mk p (ixExprAbs mp t)

ixAltn :: Ps.Altn Id.EVar -> Rn.Altn Id.EVar
ixAltn (Ps.MkAltn w p t) = ixWithPatn (MkAltn w) p t

ixDefns
  :: [Ps.Defn Id.EVar]
  -> (forall n. Vec.Vector n (Ps.Defn Id.EVar)
             -> (Ps.Expr Id.EVar -> Rn.Expr (FinScope n Id.EVar))
             -> a)
  -> a
ixDefns ds0 mk = Vec.withList ds0 $ \ds1 ->
  let mp = ifoldMap (\i d -> Map.singleton (d^.lhs) i) ds1
  in  mk ds1 (ixExprAbs mp)
