module Pukeko.MiddleEnd.Inliner where

import Pukeko.Prelude

import           Control.Lens (_1)
import qualified Data.Map.Extended as Map
-- import           Debug.Trace

import           Pukeko.AST.SuperCore
import           Pukeko.AST.Expr.Optics
import           Pukeko.AST.Name
import           Pukeko.AST.Type
import           Pukeko.MiddleEnd.CallGraph
import           Pukeko.MiddleEnd.AliasInliner (inlineSupCDecls, isLink)
-- import           Pukeko.Pretty

newtype InState = InState
  { _inlinables :: Map TmVar (FuncDecl (Only SupC))
  }

type CanInline effs = Members [State InState, NameSource] effs

type BindGroup = (BindMode, [Bind])

type CtxtExpr = ([BindGroup], Expr, [Arg])

makeLenses ''InState

makeInlinable :: CanInline effs => FuncDecl (Only SupC) -> Eff effs ()
makeInlinable supc = modifying inlinables (Map.insert (nameOf supc) supc)

copySupCDecl :: Member NameSource effs =>
  SourcePos -> FuncDecl (Only SupC) -> Eff effs (FuncDecl (Only SupC))
copySupCDecl pos (SupCDecl z t ps0 e0) = do
  let xs :: [TmVar]
      xs = toListOf (traverse . _TmPar . _1) ps0
  subst <- Map.fromList . zip xs <$> traverse (copyName pos) xs
  let ps1 = over (traverse . _TmPar . _1) (subst Map.!) ps0
  let e1 = over freeTmVar (subst Map.!) e0
  pure (SupCDecl z t ps1 e1)

matchArgs :: [(Par, Arg)] -> ([Bind], Map TyVar Type)
matchArgs pas =
  let f :: (Par, Arg) -> ([Bind], Map TyVar Type)
      f = \case
        (TmPar (x, t), TmArg e) -> ([MkBind (x, t) e], Map.empty)
        (TyPar v     , TyArg t) -> ([], Map.singleton v t)
        _ -> impossible
      (bs, subst) = foldMap f pas
  in  (over (traverse . b2binder . _2) (>>= (subst Map.!)) bs, subst)

inline :: (CanInline effs, Member (Reader SourcePos) effs) =>
  CtxtExpr -> Eff effs CtxtExpr
inline ce0@(gs0, e0, as0) =
  case e0 of
    ELet m bs0 e1 -> do
      bs1 <- (traverse . b2bound) inlineExpr bs0
      inline ((m, bs1):gs0, e1, as0)

    EApp e1 a0 -> do
      a1 <- arg2expr inlineExpr a0
      inline (gs0, e1, a1:as0)

    EVal z0 -> do
      uses inlinables (Map.lookup z0) >>= \case
        Nothing -> pure ce0

        Just supc@(SupCDecl _z0 _t0 ps1 e1)
          -- We only inline CAFs if they are links.
          | null ps1, EVal{} <- e1  -> inline (gs0, e1, as0)
          | null ps1                -> pure ce0

          -- We cannot simplify partial applications.
          | length ps1 > length as0 -> pure ce0

          | otherwise -> do
              pos <- ask @SourcePos
              copySupCDecl pos supc >>= \case
                SupCDecl _z0 _t0 ps1 e1 -> do
                  let (as1, as2) = splitAt (length ps1) as0
                      (bs, subst) = matchArgs (zip ps1 as1)
                      e2 = over expr2type (>>= (subst Map.!)) e1
                  inline ((BindPar, bs):gs0, e2, as2)

    EMat t scrut altns -> do
      e1 <- EMat t <$> inlineExpr scrut <*> (traverse . altn2expr) inlineExpr altns
      pure (gs0, e1, as0)

    ECast c e1 -> do
      e2 <- inlineExpr e1
      pure (gs0, ECast c e2, as0)

    EVar{}  -> pure ce0
    EAtm{}  -> pure ce0


inlineExpr :: (CanInline effs, Member (Reader SourcePos) effs) => Expr -> Eff effs Expr
inlineExpr e0 = do
  (gs1, e1, as1) <- inline ([], e0, [])
  pure (foldl (\e (m, bs) -> mkELet m bs e) (rewindl EApp e1 as1) gs1)

inSupCDecl :: CanInline effs => FuncDecl (Only SupC) -> Eff effs (FuncDecl (Only SupC))
inSupCDecl decl = func2expr inlineExpr decl & runReader (getPos decl)

inSCC :: CanInline effs => SCC (FuncDecl (Only SupC)) -> Eff effs Module
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

inlineModule :: Member NameSource effs => Module -> Eff effs Module
inlineModule  = mod2supcs $ \supcs0 -> do
  let sccs = scc (makeCallGraph' supcs0)
      st0  = InState mempty
  mod1 <- evalState st0 (fold <$> traverse inSCC sccs)
  pure (_mod2supcs mod1)
