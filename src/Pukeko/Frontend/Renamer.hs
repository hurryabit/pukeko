{-# LANGUAGE ViewPatterns #-}
-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.FrontEnd.Renamer
  ( renameModule
  ) where

import Pukeko.Prelude

import           Control.Lens.Indexed
import           Data.Bitraversable
import qualified Data.Map          as Map
import qualified Data.Set          as Set

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Surface    as Ps
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type

type Out = St.Renamer

renameModule :: Ps.Package -> Either Failure (Module Out)
renameModule (Ps.MkPackage _ modules) = runRn $ do
  let ldecls = concatMap Ps._mod2decls modules
  MkModule <$> traverse rnDecl ldecls

type RnEnv ev = Map Id.EVar ev
type RnState = Set Id.EVar

type Rn ev = Eff [Reader (RnEnv ev), State RnState, Reader SourcePos, Error Failure]

runRn :: Rn Void a -> Either Failure a
runRn = run . runError . runReader noPos . evalState st0 . runReader env0
  where
    st0 = mempty
    env0 = mempty

localize :: Map Id.EVar i -> Rn (EScope i ev) a -> Rn ev a
localize bs = local' upd
  where
    upd env = Map.mapWithKey (flip mkBound) bs `Map.union` Map.map Free env

-- TODO: Make @\(Ps.MkDefn x _) -> x@ a function and use it.
localizeDefns :: FoldableWithIndex Int t =>
  t (Ps.Defn _) -> Rn (EScope Int ev) a -> Rn ev a
localizeDefns =
  localize . ifoldMap (\i (Ps.MkDefn (unlctd -> x) _) -> Map.singleton x i)

rnDecl :: Ps.Decl -> Rn Void (Decl Out)
rnDecl = \case
  Ps.DType ts -> DType <$> traverse rnTConDecl ts
  Ps.DSign s -> DSign <$> rnSignDecl mempty s
  Ps.DClss (Ps.MkClssDecl c v ms0) -> do
    let env = Map.singleton v (mkBound 0 v)
    ms1 <- traverse (rnSignDecl env) ms0
    modify (<> setOf (traverse . sign2func . lctd) ms1)
    pure (DClss (MkClssDecl c v ms1))
  Ps.DInst (Ps.MkInstDecl c t vs0 qs ds0) -> do
    qvs <- rnTypeCstr vs0 qs
    ds1 <- traverse rnDefn ds0
    pure (DInst (MkInstDecl c t qvs ds1))
  Ps.DDefn d -> DDefn <$> rnDefn d
  Ps.DExtn (Ps.MkExtnDecl z s) -> do
    modify (<> Set.singleton (z^.lctd))
    pure (DExtn (MkExtnDecl (MkBind z NoType) s))

rnTConDecl :: Ps.TConDecl -> Rn ev TConDecl
rnTConDecl (Ps.MkTConDecl tcon prms0 dcons) = do
  MkTConDecl tcon prms0 <$>
    bitraverse
      (rnType (finRenamer prms0))
      (zipWithM (\tag -> rnDConDecl (tcon^.lctd) prms0 tag) [0..])
      dcons

rnDConDecl :: Id.TCon -> [Id.TVar] -> Int -> Ps.DConDecl -> Rn ev DConDecl
rnDConDecl tcon vs tag (Ps.MkDConDecl dcon flds) = here dcon $
  MkDConDecl tcon dcon tag <$> traverse (rnType env) flds
  where
    env = finRenamer vs

rnSignDecl :: Map Id.TVar tv -> Ps.SignDecl -> Rn ev (SignDecl tv)
rnSignDecl env (Ps.MkSignDecl z t) = do
  modify (Set.insert (z^.lctd))
  MkSignDecl z <$> rnTypeScheme env t

rnType :: Map Id.TVar tv -> Ps.Type -> Rn ev (Type tv)
rnType env = go
  where
    go = \case
      Ps.TVar x
        | Just v <- x `Map.lookup` env -> pure (TVar v)
        | otherwise -> throwHere ("unknown type variable:" <+> pretty x)
      Ps.TCon c -> pure (TCon c)
      Ps.TArr   -> pure TArr
      Ps.TApp tf tp -> TApp <$> go tf <*> go tp

rnTypeCstr :: [Id.TVar] -> Ps.TypeCstr -> Rn ev [QVar]
rnTypeCstr vs (Ps.MkTypeCstr qs) = do
  -- FIXME: Fail if there are constraints on variables that are not in @vs0@. In
  -- the worst case they could constrain the type variable of a class
  -- declaration.
  let mp = foldl (\acc (c, v) -> Map.insertWith (<>) v (Set.singleton c) acc) mempty qs
  pure (fmap (\v -> MkQVar (Map.findWithDefault mempty v mp) v) vs)

rnTypeScheme :: Map Id.TVar tv -> Ps.TypeScheme -> Rn ev (Type tv)
rnTypeScheme env (Ps.MkTypeScheme qs t) = do
  let vs0 = toList (setOf Ps.type2tvar t `Set.difference` Map.keysSet env)
  mkTUni <$> rnTypeCstr vs0 qs <*> rnType (fmap weakenScope env <> finRenamer vs0) t

rnCoercion :: Ps.Coercion -> Coercion (NoType tv)
rnCoercion (Ps.MkCoercion dir0 tcon) = MkCoercion dir1 tcon NoType NoType
  where
    dir1 = case dir0 of
      Ps.Inject  -> Inject
      Ps.Project -> Project

rnDefn :: Ps.Defn Id.EVar -> Rn ev (Defn Out tv ev)
rnDefn (Ps.MkDefn b e) = MkDefn (rnBind b) <$> rnExpr e

rnExpr :: Ps.Expr Id.EVar -> Rn ev (Expr Out tv ev)
rnExpr = \case
  Ps.ELoc le -> here le $ ELoc <$> lctd rnExpr le
  Ps.EVar x -> do
    y_mb <- asks (x `Map.lookup`)
    case y_mb of
      Just y -> pure (EVar y)
      Nothing -> do
        global <- gets (x `Set.member`)
        if global
          then pure (EVal x)
          else throwHere ("unknown variable:" <+> pretty x)
  Ps.ECon c -> pure (ECon c)
  Ps.ENum n -> pure (ENum n)
  Ps.EApp e0  es -> EApp <$> rnExpr e0 <*> traverse rnExpr es
  Ps.EMat e0  as0 ->
    case as0 of
      []   -> throwHere "pattern match without alternatives"
      a:as -> EMat <$> rnExpr e0 <*> traverse rnAltn (a :| as)
  Ps.ELam bs0 e0 -> do
    let bs1 = fmap rnBind bs0
    let bs2 = ifoldMap (\i (MkBind (unlctd -> x) NoType) -> Map.singleton x i) bs1
    ELam bs1 <$> localize bs2 (rnExpr e0) <*> pure NoType
  Ps.ELet (toList -> ds0) e0 ->
    ELet <$> traverse rnDefn ds0 <*> localizeDefns ds0 (rnExpr e0)
  Ps.ERec (toList -> ds0) e0 ->
    localizeDefns ds0 $ ERec <$> traverse rnDefn ds0 <*> rnExpr e0
  Ps.ECoe c e -> ECoe (rnCoercion c) <$> rnExpr e

rnBind :: Lctd Id.EVar -> Bind NoType tv
rnBind x = MkBind x NoType

rnAltn :: Ps.Altn Id.EVar -> Rn ev (Altn Out tv ev)
rnAltn (Ps.MkAltn p0 e) = do
  let p1 = rnPatn p0
  let bs = Map.fromSet id (setOf patn2evar p1)
  MkAltn p1 <$> localize bs (rnExpr e)

rnPatn :: Ps.Patn -> Patn NoType tv
rnPatn = \case
  Ps.PWld      -> PWld
  Ps.PVar x    -> PVar x
  Ps.PCon c ps -> PCon c [] (map rnPatn ps)
