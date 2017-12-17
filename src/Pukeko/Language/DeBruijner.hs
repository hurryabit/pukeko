{-# LANGUAGE TupleSections #-}
-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.Language.DeBruijner
  ( Module
  , indexModule
  )
  where

import           Control.Lens
import           Data.Map      (Map)
import qualified Data.Map      as Map
import qualified Data.Vector.Sized as V

import           Pukeko.Language.Base.AST
import           Pukeko.Language.DeBruijner.AST
import qualified Pukeko.Language.Parser.AST     as P
import qualified Pukeko.Language.Ident          as Id

indexModule :: P.Module -> Module
indexModule = map ixTopLevel

ixTopLevel :: P.TopLevel -> TopLevel
ixTopLevel top = case top of
  P.TypDef p ts  -> TypDef p ts
  P.Val    p x t -> Val    p x t
  P.TopLet p ds  -> ixDefns ds $ \ds1 _   -> TopLet p (fmap (rhs2 %~ ixExpr) ds1)
  P.TopRec p ds  -> ixDefns ds $ \ds1 abs -> TopRec p (fmap (rhs2 %~ abs)    ds1)
  P.Asm    p x a -> Asm    p x a

ixExpr :: P.Expr Id.EVar -> Expr Id.EVar
ixExpr expr = case expr of
  P.Var p x      -> Var p x
  P.Con p c      -> Con p c
  P.Num p n      -> Num p n
  P.App p t  us  -> App p (ixExpr t) (map ixExpr us)
  -- P.If  p t  u v -> If  p (ixExpr t) (ixExpr u) (ixExpr v)
  P.Mat p ts as  -> Mat p (ixExpr ts) (map ixAltn as)
  P.Lam p bs t   -> ixWithBinds (Lam p) bs t
  P.Let w ds t -> ixDefns ds $ \ds1 abs -> Let w (fmap (rhs2 %~ ixExpr) ds1) (abs t)
  P.Rec w ds t -> ixDefns ds $ \ds1 abs -> Rec w (fmap (rhs2 %~ abs)    ds1) (abs t)

ixExprAbs :: Map Id.EVar i -> P.Expr Id.EVar -> Expr (Scope i Id.EVar)
ixExprAbs mp =
  let f x = fmap (,x) (x `Map.lookup` mp)
  in  abstract f . ixExpr

ixWithBinds
  :: (forall n. V.Vector n Bind -> Expr (FinScope n Id.EVar) -> a)
  -> [Bind]
  -> P.Expr Id.EVar
  -> a
ixWithBinds mk bs0 t = V.withList bs0 $ \bs1 ->
  let mp = ifoldMap (\i -> maybe mempty (\x -> Map.singleton x i) . bindName) bs1
  in  mk bs1 (ixExprAbs mp t)

ixWithPatn ::
  (Patn -> Expr (Scope Id.EVar Id.EVar) -> a) ->
  P.Patn                                      ->
  P.Expr Id.EVar                              ->
  a
ixWithPatn mk p t =
  let mp = Map.fromList $ map (\x -> (x, x)) $ toListOf (patnBind . _Name . _2) p
  in  mk p (ixExprAbs mp t)

ixAltn :: P.Altn Id.EVar -> Altn Id.EVar
ixAltn (P.MkAltn w p t) = ixWithPatn (MkAltn w) p t

ixDefns
  :: [P.Defn Id.EVar]
  -> (forall n. V.Vector n (P.Defn Id.EVar)
             -> (P.Expr Id.EVar -> Expr (FinScope n Id.EVar))
             -> a)
  -> a
ixDefns ds0 mk = V.withList ds0 $ \ds1 ->
  let mp = ifoldMap (\i d -> Map.singleton (d^.lhs) i) ds1
  in  mk ds1 (ixExprAbs mp)
