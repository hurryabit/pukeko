{-# LANGUAGE TemplateHaskell #-}
module Pukeko.MiddleEnd.Inliner where

import Pukeko.Prelude

import           Control.Lens        (_1, matching)
import           Control.Lens.Extras (is)
import           Data.Either.Extra   (fromRight')
import qualified Data.Map.Extended   as Map
-- import           Debug.Trace

import           Pukeko.AST.SuperCore
import           Pukeko.AST.Expr.Optics
import           Pukeko.AST.Name
import           Pukeko.AST.Type
import           Pukeko.MiddleEnd.CallGraph
import           Pukeko.MiddleEnd.AliasInliner (inlineSupCDecls)
import qualified Safe

newtype InState = InState
  { _inlinables :: Map TmVar (FuncDecl (Only SupC))
  }

type CanInline effs = Members [State InState, NameSource] effs

type BindGroup = (BindMode, [Bind])

type CtxtExpr = ([BindGroup], Expr, [Arg])

data InlineMode
  = Normal
  | Scrutinee  -- inline CAFs when in WHNF into case scrutinees

makeLenses ''InState

simplifyLets :: [BindGroup] -> (Map TmVar Expr, [BindGroup])
simplifyLets = mapAccumR step Map.empty
  where
    step :: Map TmVar Expr -> BindGroup -> (Map TmVar Expr, BindGroup)
    step subst0 (m, bs0) = case m of
      BindRec -> (subst0, (m, bs0))
      BindPar ->
        let f b@(MkBind (x, _) e) = case e of
              EVar y -> Right (x, Map.findWithDefault e y subst0)
              EAtm{} -> Right (x, e)
              _      -> Left  b
            (bs1, subst1) = partitionEithers (map f bs0)
        in  (Map.unionWith impossible subst0 (Map.fromList subst1), (m, bs1))

initCtxt :: Expr -> CtxtExpr
initCtxt e = ([], e, [])

closeCtxt :: CtxtExpr -> Expr
closeCtxt (gs0, e0, as0) =
  let (subst, gs1) = simplifyLets gs0
      e1 = foldl (\e (m, bs) -> mkELet m bs e) (rewindl EApp e0 as0) gs1
  in  substitute (\x -> Map.findWithDefault (EVar x) x subst) e1

makeInlinable :: CanInline effs => FuncDecl (Only SupC) -> Eff effs ()
makeInlinable supc = modifying inlinables (Map.insert (nameOf supc) supc)

copySupCDecl :: Member NameSource effs =>
  SourcePos -> FuncDecl (Only SupC) -> Eff effs (FuncDecl (Only SupC))
copySupCDecl pos (SupCDecl z t ps0 e0) = do
  let xs :: [TmVar]
      xs = toListOf (traverse . _TmPar . _1) ps0
  subst <- Map.fromList . zip xs <$> traverse (fmap (set namePos pos) . copyName) xs
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

findAltn :: TmCon -> [Altn] -> Altn
findAltn tmCon = Safe.findJust (\(MkAltn (PSimple tmCon' _) _) -> tmCon == tmCon')

inline :: (CanInline effs, Member (Reader SourcePos) effs) =>
  InlineMode -> CtxtExpr -> Eff effs CtxtExpr
inline mode ce0@(gs0, e0, as0) =
  case e0 of
    ELet m bs0 e1 -> do
      bs1 <- (traverse . b2bound) inlineExpr bs0
      inline mode ((m, bs1):gs0, e1, as0)

    EApp e1 a0 -> do
      a1 <- arg2expr inlineExpr a0
      inline mode (gs0, e1, a1:as0)

    EVal z0 ->
      uses inlinables (Map.lookup z0) >>= \case
        Nothing -> pure ce0

        Just supc@(SupCDecl _z0 _t0 ps1 e1)
          -- We inline CAFs if they are links or in WHNF when we are in 'Scrutinee' mode
          | null ps1, EVal{} <- e1 -> inline mode (gs0, e1, as0)

          | Scrutinee <- mode
          , null ps1
          , (ECon{}, args) <- unwindl _EApp e1
          , all (\arg -> is _TyArg arg || is (_TmArg . _EAtm) arg) args ->
              inline mode (gs0, e1, as0)

          | null ps1 -> pure ce0

          -- We cannot simplify partial applications.
          | length ps1 > length as0 -> pure ce0

          | otherwise -> do
              pos <- ask @SourcePos
              copySupCDecl pos supc >>= \case
                SupCDecl _z0 _t0 ps1 e1 -> do
                  let (as1, as2) = splitAt (length ps1) as0
                      (bs, subst) = matchArgs (zip ps1 as1)
                      e2 = over expr2type (>>= (subst Map.!)) e1
                  inline mode ((BindPar, bs):gs0, e2, as2)

    EMat t scrut altns -> do
      ce1@(gs1, e1, as1) <- inline Scrutinee (initCtxt scrut)
      case e1 of
        ECon tmCon -> do
          let MkAltn (PSimple _ binders) rhs = findAltn tmCon (toList altns)
          let args =
                fromRight' . traverse (matching _TmArg) . dropWhile (is _TyArg) $ as1
          let bs =
                catMaybes (zipWithExact (\b a -> MkBind <$> b <*> pure a) binders args)
          inline mode ((BindPar, bs):gs1 ++ gs0, rhs, as0)

        _ -> do
          e2 <- EMat t (closeCtxt ce1) <$> (traverse . altn2expr) inlineExpr altns
          pure (gs0, e2, as0)

    ECast c e1 -> do
      e2 <- inlineExpr e1
      pure (gs0, ECast c e2, as0)

    EVar{}  -> pure ce0
    EAtm{}  -> pure ce0

inlineExpr :: (CanInline effs, Member (Reader SourcePos) effs) => Expr -> Eff effs Expr
inlineExpr = fmap closeCtxt . inline Normal . initCtxt

inSupCDecl :: CanInline effs => FuncDecl (Only SupC) -> Eff effs (FuncDecl (Only SupC))
inSupCDecl decl = func2expr inlineExpr decl & runReader (getPos decl)

isConCAF :: FuncDecl (Only SupC) -> Bool
isConCAF = \case
  SupCDecl _ _ [] (unwindl _EApp -> (ECon{}, _)) -> True
  _ -> False

inSCC :: CanInline effs => SCC (FuncDecl (Only SupC)) -> Eff effs Module
inSCC = \case
  -- TODO: We need a proper loop breaker.
  CyclicSCC supcs0 -> do
    for_ supcs0 $ \supc -> when (isConCAF supc) (makeInlinable supc)
    supcs1 <- traverse inSupCDecl supcs0
    let supcs2 = inlineSupCDecls supcs1
    -- Since CAFs are only inlined under very specific conditions, we can safely
    -- make even recursive CAFs inlinable.
    for_ supcs2 $ \supc -> when (null (_supc2params supc)) (makeInlinable supc)
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
