-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.FrontEnd.Renamer
  ( renameModule
  ) where

import Pukeko.Prelude

import qualified Data.Finite       as Fin
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Vector.Sized as Vec

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Surface    as Ps
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type

type Out = St.Renamer

renameModule :: Ps.Package -> Either Doc (Module Out)
renameModule (Ps.MkPackage _ modules) = runRn $ do
  let ldecls = concatMap Ps._mod2decls modules
  MkModule <$> concat <$> traverse rnDecl ldecls

type RnEnv ev = Map Id.EVar ev
type RnState = Set Id.EVar

type Rn ev = Eff [Reader (RnEnv ev), State RnState, Reader Pos, Error Doc]

runRn :: Rn Void a -> Either Doc a
runRn = run . runError . runReader noPos . evalState st0 . runReader env0
  where
    st0 = mempty
    env0 = mempty

localize :: Map Id.EVar i -> Rn (EScope i ev) a -> Rn ev a
localize bs = local' upd
  where
    upd env = Map.mapWithKey (flip mkBound) bs `Map.union` Map.map Free env

-- TODO: Make @\(Ps.MkDefn x _) -> x@ a function and use it.
localizeDefns :: Vector n (Lctd (Ps.Defn _)) -> Rn (EFinScope n ev) a -> Rn ev a
localizeDefns = localize . ifoldMap (\i (Lctd _ (Ps.MkDefn x _)) -> Map.singleton x i)

rnDecl :: Lctd Ps.Decl -> Rn Void [Lctd (Decl Out)]
rnDecl (Lctd pos decl) = here pos $ case decl of
  Ps.DType ts -> (:[]) . Lctd pos . DType <$> (traverse . lctd) rnTConDecl ts
  Ps.DSign s -> (:[]) . Lctd pos . DSign <$> rnSignDecl mempty s
  Ps.DClss (Ps.MkClssDecl c v ms0) -> do
    let env = Map.singleton v (mkBound Fin.zero v)
    ms1 <- traverse (rnSignDecl env) ms0
    modify (<> setOf (traverse . sign2func) ms1)
    pure [Lctd pos (DClss (MkClssDecl c v ms1))]
  Ps.DInst (Ps.MkInstDecl c t vs0 qs ds0) -> do
    Vec.withList vs0 $ \vs1 -> do
      qvs <- rnTypeCstr vs1 qs
      ds1 <- (traverse . lctd) rnDefn ds0
      pure [Lctd pos (DInst (MkInstDecl c t qvs ds1))]
  Ps.DLet ds0 -> do
    ds1 <- (traverse . lctd) rnDefn ds0
    modify (<> setOf (traverse . traverse . to (\(Ps.MkDefn x _) -> x)) ds0)
    pure ((fmap . fmap) DDefn (toList ds1))
  Ps.DRec ds0 -> do
    modify  (<> setOf (traverse . traverse . to (\(Ps.MkDefn x _) -> x)) ds0)
    ds1 <- (traverse . lctd) rnDefn ds0
    pure ((fmap . fmap) DDefn (toList ds1))
  Ps.DPrim (Ps.MkPrimDecl z s) -> do
    modify (<> Set.singleton z)
    pure [Lctd pos (DPrim (MkPrimDecl (MkBind z NoType) s))]

rnTConDecl :: Ps.TConDecl -> Rn ev (Some1 TConDecl)
rnTConDecl (Ps.MkTConDecl tcon prms0 dcons) = Vec.withList prms0 $ \prms1 -> do
  Some1 <$> MkTConDecl tcon prms1
    <$> zipWithM (\tag -> lctd (rnDConDecl tcon prms1 tag)) [0..] dcons

rnDConDecl :: Id.TCon -> Vector n Id.TVar -> Int -> Ps.DConDecl -> Rn ev (DConDecl n)
rnDConDecl tcon vs tag (Ps.MkDConDecl dcon flds) =
  MkDConDecl tcon dcon tag <$> traverse (rnType env) flds
  where
    env = finRenamer vs

rnSignDecl :: Map Id.TVar tv -> Ps.SignDecl -> Rn ev (SignDecl tv)
rnSignDecl env (Ps.MkSignDecl z t) = MkSignDecl z <$> rnTypeScheme env t

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

rnTypeCstr :: Vector n Id.TVar -> Ps.TypeCstr -> Rn ev (Vector n QVar)
rnTypeCstr vs (Ps.MkTypeCstr qs) = do
  -- FIXME: Fail if there are constraints on variables that are not in @vs0@. In
  -- the worst case they could constrain the type variable of a class
  -- declaration.
  let mp = foldl (\acc (c, v) -> Map.insertWith (<>) v (Set.singleton c) acc) mempty qs
  pure (fmap (\v -> MkQVar (Map.findWithDefault mempty v mp) v) vs)

rnTypeScheme :: Map Id.TVar tv -> Ps.TypeScheme -> Rn ev (Type tv)
rnTypeScheme env (Ps.MkTypeScheme qs t) = do
  let vs0 = setOf Ps.type2tvar t `Set.difference` Map.keysSet env
  Vec.withList (toList vs0) $ \vs1 ->
    mkTUni <$> rnTypeCstr vs1 qs <*> rnType (fmap weaken env <> finRenamer vs1) t

rnDefn :: Ps.Defn Id.EVar -> Rn ev (Defn Out tv ev)
rnDefn (Ps.MkDefn b e) = MkDefn (rnBind b) <$> rnExpr e

rnExpr :: Ps.Expr Id.EVar -> Rn ev (Expr Out tv ev)
rnExpr = \case
  Ps.ELoc l -> ELoc <$> lctd rnExpr l
  Ps.EVar x -> do
    y_mb <- asks (x `Map.lookup`)
    case y_mb of
      Just y -> pure (EVar y)
      Nothing -> do
        global <- gets (x `Set.member`)
        if global then pure (EVal x) else throwHere ("unknown variable:" <+> pretty x)
  Ps.ECon c -> pure (ECon c)
  Ps.ENum n -> pure (ENum n)
  Ps.EApp e0  es -> EApp <$> rnExpr e0 <*> traverse rnExpr es
  Ps.EMat e0  as0 ->
    case as0 of
      []   -> throwHere "pattern match without alternatives"
      a:as -> EMat <$> rnExpr e0 <*> traverse rnAltn (a :| as)
  Ps.ELam bs0 e0 -> Vec.withNonEmpty (fmap rnBind bs0) $ \bs1 -> do
    let bs2 = ifoldMap (\i (MkBind x NoType) -> Map.singleton x i) bs1
    ELam bs1 <$> localize bs2 (rnExpr e0) <*> pure NoType
  Ps.ELet ds0 e0 -> Vec.withNonEmpty ds0 $ \ds1 -> do
    ELet <$> (traverse . lctd) rnDefn ds1 <*> localizeDefns ds1 (rnExpr e0)
  Ps.ERec ds0 e0 -> Vec.withNonEmpty ds0 $ \ds1 -> do
    localizeDefns ds1 $ ERec <$> (traverse . lctd) rnDefn ds1 <*> rnExpr e0

rnBind :: Id.EVar -> Bind NoType tv
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
