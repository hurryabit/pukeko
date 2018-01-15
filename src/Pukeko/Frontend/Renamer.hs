-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.FrontEnd.Renamer
  ( renameModule
  ) where

import Pukeko.Prelude

import           Control.Lens
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

renameModule :: MonadError String m => Ps.Package -> m (Module Out)
renameModule (Ps.MkPackage _ modules) =
  runRn $ MkModule <$> concat <$> traverse rnDecl (concatMap Ps._mod2decls modules)

type RnEnv ev = Map Id.EVar ev
type RnState = Set Id.EVar

newtype Rn ev a = Rn{unRn :: ReaderT (RnEnv ev) (StateT RnState (Except String)) a}
  deriving ( Functor, Applicative, Monad
           , MonadReader (RnEnv ev)
           , MonadState  RnState
           , MonadError String
           )

runRn :: MonadError String m => Rn Void a -> m a
runRn ix = runExcept (evalStateT (runReaderT (unRn ix) Map.empty) Set.empty)

localize :: Map Id.EVar i -> Rn (EScope i ev) a -> Rn ev a
localize bs = Rn . withReaderT upd . unRn
  where
    upd env = Map.mapWithKey (flip mkBound) bs `Map.union` Map.map Free env

localizeDefns :: Vector n (Ps.Defn _) -> Rn (EFinScope n ev) a -> Rn ev a
localizeDefns =
  localize . ifoldMap (\i (Ps.MkDefn (Ps.MkBind _ x) _) -> Map.singleton x i)

rnDecl :: Ps.Decl -> Rn Void [Decl Out]
rnDecl top = case top of
  Ps.DType ts ->
    (:[]) . DType <$> for ts (\t -> here (Ps._tcon2pos t) (rnTConDecl t))
  Ps.DSign s -> (:[]) . DSign <$> rnSignDecl mempty s
  Ps.DClss (Ps.MkClssDecl w c v ms0) -> do
    let env = Map.singleton v (mkBound Fin.zero v)
    ms1 <- traverse (rnSignDecl env) ms0
    id <>= setOf (traverse . sign2func) ms1
    pure [DClss (MkClssDecl w c v ms1)]
  Ps.DInst (Ps.MkInstDecl w c t vs0 qs ds0) -> do
    Vec.withList vs0 $ \vs1 -> do
      qvs <- rnTypeCstr vs1 qs
      ds1 <- traverse rnDefn ds0
      pure [DInst (MkInstDecl w c t qvs ds1)]
  Ps.DLet ds0 -> do
    ds1 <- traverse rnDefn ds0
    let xs = fmap (\(Ps.MkDefn (Ps.MkBind _ x) _) -> x) ds0
    id <>= setOf traverse xs
    pure (map DDefn (toList ds1))
  Ps.DRec ds0 -> do
    let xs = fmap (\(Ps.MkDefn (Ps.MkBind _ x) _) -> x) ds0
    id <>= setOf traverse xs
    ds1 <- traverse rnDefn ds0
    pure (map DDefn (toList ds1))
  Ps.DPrim (Ps.MkPrimDecl w z s) -> do
    id <>= Set.singleton z
    pure [DPrim (MkPrimDecl (MkBind w z NoType) s)]

rnTConDecl :: Ps.TConDecl -> Rn ev (Some1 TConDecl)
rnTConDecl (Ps.MkTConDecl w tcon prms0 dcons) = Vec.withList prms0 $ \prms1 -> do
  Some1 <$> MkTConDecl w tcon prms1 <$> zipWithM (rnDConDecl tcon prms1) [0..] dcons

rnDConDecl :: Id.TCon -> Vector n Id.TVar -> Int -> Ps.DConDecl -> Rn ev (DConDecl n)
rnDConDecl tcon vs tag (Ps.MkDConDecl w dcon flds) =
  MkDConDecl w tcon dcon tag <$> traverse (rnType env) flds
  where
    env = finRenamer vs

rnSignDecl :: Map Id.TVar tv -> Ps.SignDecl -> Rn ev (SignDecl tv)
rnSignDecl env (Ps.MkSignDecl w z t) = MkSignDecl w z <$> rnTypeScheme env t

rnType :: Map Id.TVar tv -> Ps.Type -> Rn ev (Type tv)
rnType env = go
  where
    go = \case
      Ps.TVar x
        | Just v <- x `Map.lookup` env -> pure (TVar v)
        | otherwise                    -> throw "unknown type variable" x
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
  Ps.EVar w x -> do
    y_mb <- view (at x)
    case y_mb of
      Just y -> pure (EVar w y)
      Nothing -> do
        global <- use (contains x)
        if global then pure (EVal w x) else throwAt w "unknown variable" x
  Ps.ECon w c -> pure (ECon w c)
  Ps.ENum w n -> pure (ENum w n)
  Ps.EApp w e0  es -> EApp w <$> rnExpr e0 <*> traverse rnExpr es
  Ps.EMat w e0  as0 ->
    case as0 of
      []   -> throwErrorAt w "pattern match without alternatives"
      a:as -> EMat w <$> rnExpr e0 <*> traverse rnAltn (a :| as)
  Ps.ELam w bs0 e0 -> Vec.withNonEmpty (fmap rnBind bs0) $ \bs1 -> do
    let bs2 = ifoldMap (\i (MkBind _ x NoType) -> Map.singleton x i) bs1
    ELam w bs1 <$> localize bs2 (rnExpr e0) <*> pure NoType
  Ps.ELet w ds0 e0 -> Vec.withNonEmpty ds0 $ \ds1 -> do
    ELet w <$> traverse rnDefn ds1 <*> localizeDefns ds1 (rnExpr e0)
  Ps.ERec w ds0 e0 -> Vec.withNonEmpty ds0 $ \ds1 -> do
    localizeDefns ds1 $ ERec w <$> traverse rnDefn ds1 <*> rnExpr e0

rnBind :: Ps.Bind -> Bind NoType tv
rnBind (Ps.MkBind w x) = MkBind w x NoType

rnAltn :: Ps.Altn Id.EVar -> Rn ev (Altn Out tv ev)
rnAltn (Ps.MkAltn w p0 e) = do
  let p1 = rnPatn p0
  let bs = Map.fromSet id (setOf patn2evar p1)
  MkAltn w p1 <$> localize bs (rnExpr e)

rnPatn :: Ps.Patn -> Patn NoType tv
rnPatn = \case
  Ps.PWld w      -> PWld w
  Ps.PVar w x    -> PVar w x
  Ps.PCon w c ps -> PCon w c [] (map rnPatn ps)
