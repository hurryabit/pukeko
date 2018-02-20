module Pukeko.MiddleEnd.Inliner where

-- import Pukeko.Prelude

-- import qualified Data.Map    as Map
-- import qualified Data.Vector as Vec
-- import           Debug.Trace

-- import           Pukeko.AST.SuperCore
-- import           Pukeko.AST.Expr.Optics
-- import           Pukeko.AST.Name
-- import           Pukeko.AST.Type
-- import           Pukeko.MiddleEnd.CallGraph
-- import           Pukeko.MiddleEnd.AliasInliner (inlineSupCDecls, isLink)
-- import           Pukeko.Pretty

-- data InState = InState
--   { _inlinables :: Map (Name EVar) (FuncDecl (Only SupC))
--   }

-- type In = Eff '[State InState]

-- makeLenses ''InState

-- makeInlinable :: FuncDecl (Only SupC) -> In ()
-- makeInlinable supc = modifying inlinables (Map.insert (nameOf supc) supc)

-- unwind :: Expr tv ev -> (Expr tv ev, [Type tv], [Expr tv ev])
-- unwind e0 =
--   let (e1, as) = unwindEApp e0
--       (e2, ts) = goT [] e1
--   in  (e2, ts, as)
--   where
--     goT :: [[Type tv]] -> Expr tv ev -> (Expr tv ev, [Type tv])
--     goT tss = \case
--       ETyApp e ts -> goT (toList ts:tss) e
--       e           -> (e, concat tss)

-- -- | Replace a reference to an alias by a reference to the target of the alias.
-- inEVal :: Name EVar -> In (Expr tv ev)
-- inEVal z0 = do
--   fdecl_mb <- uses inlinables (Map.lookup z0)
--   case fdecl_mb of
--     Just (SupCDecl _z0 _t0 [] [] (EVal z1)) -> inEVal z1
--     _ -> pure (EVal z0)

-- inRedex :: forall tv ev. (BaseTVar tv, BaseEVar ev) =>
--   Expr tv ev -> In (Expr tv ev)
-- inRedex e0 = do
--   let (f, ts, as) = unwind e0
--   let continue = foldl EApp (mkETyApp f ts) <$> traverse inExpr as
--   case f of
--     EVal z0 -> do
--       supc_mb <- uses inlinables (Map.lookup z0)
--       case supc_mb of
--         Just (SupCDecl _ _ [] [] (EVal z1)) ->
--           -- trace ("INLINING: " ++ render (pretty e0))
--             (inExpr (foldl EApp (EVal z1 `mkETyApp` ts) as))
--         Just (SupCDecl _ _ vs xs e1)
--           -- NOTE: We don't want to inline CAFs. That's why we ensure @1 <= n@.
--           | length vs == length ts && 1 <= n && n <= length as -> do
--               let (as0, as1) = splitAt n as
--               let tsV = Vec.fromList ts
--               let defns = zipWith
--                           (MkDefn . over bind2type (instantiate' (tsV Vec.!)))
--                           xs as0
--               let e2 :: Expr (TScope Int tv) (EScope Int ev)
--                   e2 = bimap (over _Free absurd) (over _Free absurd) e1
--               let inst :: Traversable s => Type (s (TScope Int tv)) -> Type (s tv)
--                   inst t = t >>= traverse (scope pure (tsV Vec.!))
--               let body :: Expr tv (EScope Int ev)
--                   body = runIdentity (expr2type (Identity . inst) e2)
--               let let_ = ELet defns body
--               -- trace ("INLINING: " ++ render (pretty e0))
--               (inExpr (foldl EApp let_ as1))
--           where n = length xs
--         Nothing -> continue
--         _ -> trace ("NOT INLINING: " ++ render (pretty e0)) continue
--     _ -> continue

-- inExpr :: forall tv ev. (BaseTVar tv, BaseEVar ev) =>
--   Expr tv ev -> In (Expr tv ev)
-- inExpr e0 = case e0 of
--   EVal{}     -> inRedex e0
--   EApp{}     -> inRedex e0
--   EVar x     -> pure (EVar x)
--   ECon c     -> pure (ECon c)
--   ENum n     -> pure (ENum n)
--   EApp e  a  -> EApp <$> inExpr e <*> inExpr a
--   EMat t  cs -> EMat <$> inExpr t <*> (traverse . altn2expr) inExpr cs
--   ELet ds t  -> ELet <$> (traverse . defn2expr) inExpr ds <*> inExpr t
--   ERec ds t  -> ERec <$> (traverse . defn2expr) inExpr ds <*> inExpr t
--   ETyCoe d e -> ETyCoe d <$> inExpr e
--   ETyAbs x e -> ETyAbs x <$> inExpr e
--   ETyApp{}   -> inRedex e0
--   ETyAnn t e -> ETyAnn t <$> inExpr e

-- inSupCDecl :: FuncDecl (Only SupC) -> In (FuncDecl (Only SupC))
-- inSupCDecl = func2expr inExpr

-- inSCC :: SCC (FuncDecl (Only SupC)) -> In Module
-- inSCC = \case
--   CyclicSCC supcs0 -> do
--     supcs1 <- traverse inSupCDecl supcs0
--     let supcs2 = inlineSupCDecls supcs1
--     for_ supcs2 $ \supc -> when (isJust (isLink supc)) (makeInlinable supc)
--     pure (foldMap mkFuncDecl supcs2)
--   AcyclicSCC supc0 -> do
--     supc1 <- inSupCDecl supc0
--     -- FIXME: Check if the function is actually inlinable.
--     makeInlinable supc1
--     pure (mkFuncDecl supc1)

-- inlineModule :: Module -> Module
-- inlineModule = over mod2supcs $ \supcs0 ->
--   let sccs = scc (makeCallGraph' supcs0)
--       st0  = InState mempty
--       mod1 = run (evalState st0 (fold <$> traverse inSCC sccs))
--   in  _mod2supcs mod1
