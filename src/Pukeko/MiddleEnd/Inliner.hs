module Pukeko.MiddleEnd.Inliner where

import Pukeko.Prelude

import qualified Data.Map as Map
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.SuperCore
import           Pukeko.MiddleEnd.CallGraph
import           Pukeko.MiddleEnd.AliasInliner (inlineSupCDecls, isLink)

data InState = InState
  { _inlinables :: Map Id.EVar (FuncDecl 'SupC)
  }

type In = Eff '[State InState]

makeLenses ''InState

makeInlinable :: FuncDecl 'SupC -> In ()
makeInlinable supc = modifying inlinables (Map.insert (supc^.func2name) supc)

stripELoc :: Expr tv ev -> Expr tv ev
stripELoc = \case
  ELoc l -> stripELoc (unlctd l)
  e      -> e

-- | Replace a reference to an alias by a reference to the target of the alias.
inEVal :: Id.EVar -> In (Expr tv ev)
inEVal z0 = do
  fdecl_mb <- uses inlinables (Map.lookup z0)
  case fdecl_mb of
    Just (SupCDecl _z0 [] _t [] (stripELoc -> EVal z1)) -> inEVal z1
    _ -> pure (EVal z0)

inExpr :: Expr tv ev -> In (Expr tv ev)
inExpr = \case
  ELoc e       -> ELoc <$> lctd inExpr e
  EVar x       -> pure (EVar x)
  EVal z       -> inEVal z
  ECon c       -> pure (ECon c)
  ENum n       -> pure (ENum n)
  EApp t  us   -> EApp <$> inExpr t <*> traverse inExpr us
  ECas t  cs   -> ECas <$> inExpr t <*> (traverse . case2expr) inExpr cs
  ELet ds t    -> ELet <$> (traverse . defn2expr) inExpr ds <*> inExpr t
  ERec ds t    -> ERec <$> (traverse . defn2expr) inExpr ds <*> inExpr t
  ECoe d e     -> ECoe d <$> inExpr e
  ETyAbs x e   -> ETyAbs x <$> inExpr e
  ETyApp e t   -> ETyApp <$> inExpr e <*> pure t

inSupCDecl :: FuncDecl 'SupC -> In (FuncDecl 'SupC)
inSupCDecl = func2expr inExpr

inSCC :: SCC (FuncDecl 'SupC) -> In Module
inSCC = \case
  CyclicSCC supcs0 -> do
    supcs1 <- traverse inSupCDecl supcs0
    let supcs2 = inlineSupCDecls supcs1
    for_ supcs2 $ \supc -> when (isJust (isLink supc)) (makeInlinable supc)
    pure (foldMap mkFuncDecl supcs2)
  AcyclicSCC supc0 -> do
    supc1 <- inSupCDecl supc0
    -- FIXME: Check if the function is actually inlinable.
    makeInlinable supc1
    pure (mkFuncDecl supc1)

inlineModule :: Module -> Module
inlineModule = over mod2supcs $ \supcs0 ->
  let sccs = scc (makeCallGraph' supcs0)
      st0  = InState mempty
      mod1 = run (evalState st0 (fold <$> traverse inSCC sccs))
  in  _mod2supcs mod1
