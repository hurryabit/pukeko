{-# LANGUAGE TypeApplications #-}
module Pukeko.FrontEnd.Inferencer
  ( Out
  , inferModule
  )
where

import Pukeko.Prelude

import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.ST.Class
import qualified Data.List.NonEmpty as NE
import qualified Data.Map         as Map
import           Data.STRef
import qualified Data.Vector.Sized as Vec

import           Pukeko.Pretty
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Scope      as Sc
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type       hiding ((*~>))
import           Pukeko.FrontEnd.Inferencer.UType
import qualified Pukeko.FrontEnd.Inferencer.Unify as U

type In    = St.KindChecker
type Out   = St.Inferencer Type
type Aux s = St.Inferencer (UType s)

data Environment v s = MkEnvironment
  { _locals :: Sc.EnvOf v (UType s Void)
  , _level  :: Int
  }
makeLenses ''Environment

newtype TI v s a =
  TI{unTI :: InfoT (ReaderT (Environment v s) (SupplyT Id.TVar (ExceptT String (ST s)))) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (Environment v s)
           , MonadSupply Id.TVar
           , MonadInfo
           )

instance MonadST (TI v s) where
  type World (TI v s) = s
  liftST = TI . liftST

runTI :: TI Void s a -> Module In -> ExceptT String (ST s) a
runTI tc module_ = do
  let env0 = MkEnvironment (Const ()) 0
  evalSupplyT (runReaderT (runInfoT (unTI tc) module_) env0) Id.freshTVars

freshQualUVar :: Set Id.Clss -> TI v s (UType s Void)
freshQualUVar q = do
  v <- fresh
  l <- view level
  UVar <$> liftST (newSTRef (UFree (MkQVar q v) l))

freshUVar :: TI v s (UType s Void)
freshUVar = freshQualUVar mempty

unify :: Pos -> UType s Void -> UType s Void -> TI v s ()
unify w t1 t2 = TI $ lift $ lift $ lift $ U.unify w t1 t2

localize ::
  forall i v s a.
  (HasEnvLevel i, HasEnv v) =>
  EnvLevelOf i (UType s Void) ->
  TI (EScope i v) s a ->
  TI v s a
localize ts = TI . mapInfoT (withReaderT (locals %~ Sc.extendEnv @i @v ts)) . unTI

lookupVar :: (HasEnv v) => v -> TI v s (UType s Void)
lookupVar = views locals . Sc.lookupEnv

lookupFun :: Id.EVar -> TI v s (UType s Void)
lookupFun x = open . _sign2type <$> findSign x

generalize :: UType s Void -> TI v s (UType s Void, [UType s Void])
generalize t0 = do
  (t1, qvs1) <- runWriterT (go t0)
  let qvs2 = map (\(v, q) -> MkQVar q v) (Map.toList qvs1)
  pure (mkUTUni qvs2 t1, map (UTVar . _qvar2tvar) qvs2)
  where
    go t0 = case t0 of
      (UVar uref) -> do
        uvar <- lift (liftST (readSTRef uref))
        cur_level <- view level
        case uvar of
          UFree (MkQVar q v) l
            | l > cur_level -> do
                let t1 = UTVar v
                lift (liftST (writeSTRef uref (ULink t1)))
                tell (Map.singleton v q)
                pure t1
            | otherwise     -> pure t0
          ULink t1 -> go t1
      UTVar{} -> pure t0
      UTArr{} -> pure t0
      UTCon{} -> pure t0
      UTUni{} -> bug "quantification during generalization"
      UTApp tf tp -> UTApp <$> go tf <*> go tp

instantiate :: UType s Void -> TI v s ([UType s Void], UType s Void)
instantiate ts = do
  let (qvs, t0) = unUTUni ts
  env <- traverse (\(MkQVar q v) -> (,) v <$> freshQualUVar q) qvs
  (,) (map snd env) <$> liftST (subst (Map.fromList env) t0)

instantiateTCon ::
  TConDecl n -> TI v s (UType s Void, Vector n (UType s Void))
instantiateTCon (MkTConDecl _ tcon params _dcons) = do
  t_params <- traverse (const freshUVar) params
  return (appTCon tcon (toList t_params), t_params)

inferPatn ::
  Patn NoType Void -> UType s Void -> TI v s (Patn (UType s) Void, Map Id.EVar (UType s Void))
inferPatn patn t_expr = case patn of
  PWld w   -> pure (PWld w, Map.empty)
  PVar w x -> pure (PVar w x, Map.singleton x t_expr)
  PCon w dcon (_ :: [NoType Void]) ps0 -> do
    Some1 (Pair1 tcon MkDConDecl{_dcon2name = dname, _dcon2flds = flds}) <- findDCon dcon
    when (length ps0 /= length flds) $
      throwDocAt w $ "term cons" <+> quotes (pretty dname) <+>
      "expects" <+> int (length flds) <+> "arguments"
    (t_inst, t_params) <- instantiateTCon tcon
    unify w t_expr t_inst
    let t_fields = map (open1 . fmap (scope absurd (t_params Vec.!))) flds
    (ps1, binds) <- unzip <$> zipWithM inferPatn ps0 t_fields
    pure (PCon w dcon (toList t_params) ps1, Map.unions binds)

inferLet ::
  (HasEnv v) =>
  Vector n (Defn In Void v) ->
  TI v s (Vector n (Defn (Aux s) Void v), Vector n (UType s Void))
inferLet defns0 = do
  let (ls0, rs0) = Vec.unzip (fmap (\(MkDefn l r) -> (l, r)) defns0)
  (rs1, ts1) <- Vec.unzip <$> local (level +~ 1) (traverse infer rs0)
  (ts2, _qs) <- Vec.unzip <$> traverse generalize ts1
  -- let ls1 = Vec.zipWith (set bindType) ts2 ls0
  let defns1 = Vec.zipWith3 (\t2 -> MkDefn . set bind2type t2) ts2 ls0 rs1
  pure (defns1, ts2)

inferRec ::
  (HasEnv v) =>
  Vector n (Defn In Void (EFinScope n v)) ->
  TI v s ( Vector n (Defn (Aux s) Void (EFinScope n v))
         , Vector n (UType s Void))
inferRec defns0 = do
  let (ls0, rs0) = Vec.unzip (fmap (\(MkDefn l r) -> (l, r)) defns0)
  (rs1, ts1) <- local (level +~ 1) $ do
    us <- traverse (const freshUVar) ls0
    (rs1, ts1) <- Vec.unzip <$> localize us (traverse infer rs0)
    Vec.zipWith3M_ (\r0 -> unify (r0^.expr2pos)) rs0 us ts1
    pure (rs1, ts1)
  (ts2, qs) <- Vec.unzip <$> traverse generalize ts1
  let addETyApp w = scope' (EVar w . Free)
        (\i b -> mkETyApp w (EVar w (mkBound i b)) (map (fmap absurd) (qs Vec.! i)))
  let rs2 = fmap (// addETyApp) rs1
  let defns1 = Vec.zipWith3 (\t2 -> MkDefn . set bind2type t2) ts2 ls0 rs2
  pure (defns1, ts2)

infer :: (HasEnv v) => Expr In Void v -> TI v s (Expr (Aux s) Void v, UType s Void)
infer = \case
    EVar w x -> first (mkETyApp w (EVar w x)) <$> (lookupVar x >>= instantiate)
    EVal w z -> first (mkETyApp w (EVal w z)) <$> (lookupFun z >>= instantiate)
    ECon w c -> first (mkETyApp w (ECon w c)) <$> (typeOfDCon c >>= instantiate . open)
    ENum w n -> pure (ENum w n, open typeInt)
    EApp w fun0 args0 -> do
      (fun1, t_fun) <- infer fun0
      (args1, t_args) <- NE.unzip <$> traverse infer args0
      t_res <- freshUVar
      unify w t_fun (t_args *~> t_res)
      pure (EApp w fun1 args1, t_res)
    ELam w binds0 rhs0 NoType -> do
      t_binds <- traverse (const freshUVar) binds0
      (rhs1, t_rhs) <- localize t_binds (infer rhs0)
      let binds1 = Vec.zipWith (\(MkBind w' x NoType) -> MkBind w' x) binds0 t_binds
      pure (ELam w binds1 rhs1 t_rhs, t_binds *~> t_rhs)
    ELet w defns0 rhs0 -> do
      (defns1, t_defns) <- inferLet defns0
      first (ELet w defns1) <$> localize t_defns (infer rhs0)
    ERec w defns0 rhs0 -> do
      (defns1, t_defns) <- inferRec defns0
      first (ERec w defns1) <$> localize t_defns (infer rhs0)
    EMat w expr0 altns0 -> do
      (expr1, t_expr) <- infer expr0
      t_res <- freshUVar
      altns1 <- for altns0 $ \(MkAltn w' patn0 rhs0) -> do
        (patn1, t_binds) <- inferPatn patn0 t_expr
        (rhs1, t_rhs) <- localize t_binds (infer rhs0)
        unify w t_res t_rhs
        pure (MkAltn w' patn1 rhs1)
      pure (EMat w expr1 altns1, t_res)

inferDecl :: Decl In -> TI Void s (Maybe (Decl (Aux s)))
inferDecl = \case
  DType ds -> yield (DType ds)
  DSign{} -> pure Nothing
  DDefn (MkDefn (MkBind w x NoType) e0) -> do
    reset
    -- NOTE: We do not instantiate the universally quantified type variables in
    -- prenex position of @t_decl@. Turning them into unification variables
    -- would make the following type check:
    --
    -- val f : a -> b
    -- let f = fun x -> x
    t_decl <- lookupFun x
    let (_, t0) = unUTUni t_decl
    (e1, t1) <- infer e0
    -- NOTE: This unification should bind all unification variables in @t1@. If
    -- it does not, the second quantification step will catch them.
    unify w t0 t1
    yield (DDefn (MkDefn (MkBind w x t_decl) e1))
  DPrim (MkPrimDecl (MkBind w z NoType) s) -> do
    t <- lookupFun z
    yield (DPrim (MkPrimDecl (MkBind w z t) s))
  where
    yield = pure . Just

inferModule' :: Module In -> TI Void s (Module (Aux s))
inferModule' = module2decls (\tops -> catMaybes <$> traverse inferDecl tops)

type TQEnv tv = Map Id.TVar tv

newtype TQ tv s a =
  TQ{unTQ :: ReaderT (TQEnv tv) (SupplyT Id.TVar (ExceptT String (ST s))) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (TQEnv tv)
           , MonadSupply Id.TVar
           )

instance MonadST (TQ tv s) where
  type World (TQ tv s) = s
  liftST = TQ . liftST

runTQ :: TQ Void s a -> ExceptT String (ST s) a
runTQ tq = evalSupplyT (runReaderT (unTQ tq) mempty) tvars
  where
    tvars = map (Id.tvar . (:[])) ['a' .. 'z'] ++ Id.freshTVars

localizeTQ ::
  Vector n QVar ->
  (Vector n QVar -> TQ (TFinScope n tv) s a) ->
  TQ tv s a
localizeTQ xs m = do
  ys <- traverse (qvar2tvar (const fresh)) xs
  let env1 =
        ifoldMap
          (\i (x, y) -> Map.singleton (x^.qvar2tvar) (mkBound i (y^.qvar2tvar)))
          (Vec.zip xs ys)
  let upd env0 = env1 <> fmap Free env0
  (TQ . withReaderT upd . unTQ) (m ys)

qualType :: UType s Void -> TQ tv s (Type tv)
qualType = \case
  UTVar x -> do
    y_mb <- asks (x `Map.lookup`)
    case y_mb of
      Nothing -> bugWith "unknown type variable during quantification" x
      Just y  -> pure (TVar y)
  UTArr -> pure TArr
  UTCon c -> pure (TCon c)
  UTApp t1 t2 -> TApp <$> qualType t1 <*> qualType t2
  UTUni{} -> bug "quantification during quantification"
  UVar uref -> do
    uvar <- liftST (readSTRef uref)
    case uvar of
      UFree x _ -> bugWith "free unification variable during quantification" x
      ULink t -> qualType t

qualDefn :: Defn (Aux s) Void ev -> TQ tv s (Defn Out tv ev)
qualDefn (MkDefn (MkBind w x ts) e0) = case ts of
  UTUni ys0 t0 -> Vec.withList (toList ys0) $ \ys1 -> localizeTQ ys1 $ \ys2 -> do
    t1  <- TUni ys2 <$> qualType t0
    e1  <- ETyAbs w ys2 <$> qualExpr e0
    pure (MkDefn (MkBind w x t1) e1)
  t0 -> MkDefn <$> (MkBind w x <$> qualType t0) <*> qualExpr e0

qualExpr :: Expr (Aux s) Void ev -> TQ tv s (Expr Out tv ev)
qualExpr = \case
  EVar w x -> pure (EVar w x)
  EVal w z -> pure (EVal w z)
  ECon w c -> pure (ECon w c)
  ENum w n -> pure (ENum w n)
  EApp w e0 es -> EApp w <$> qualExpr e0 <*> traverse qualExpr es
  ELam w bs0 e0 t -> do
    bs1 <- for bs0 $ \(MkBind w' x ts) -> do
      MkBind w' x <$> qualType ts
    ELam w bs1 <$> qualExpr e0 <*> qualType t
  ELet w ds e0 -> ELet w <$> traverse qualDefn ds <*> qualExpr e0
  ERec w ds e0 -> ERec w <$> traverse qualDefn ds <*> qualExpr e0
  EMat w e0 as -> EMat w <$> qualExpr e0 <*> traverse qualAltn as
  ETyAbs _ _ _ -> bug "type abstraction during quantification"
  ETyApp w e0 ts -> ETyApp w <$> qualExpr e0 <*> traverse qualType ts

qualAltn :: Altn (Aux s) Void ev -> TQ tv s (Altn Out tv ev)
qualAltn (MkAltn w p e) = MkAltn w <$> qualPatn p <*> qualExpr e

qualPatn :: Patn (UType s) Void -> TQ tv s (Patn Type tv)
qualPatn = \case
  PWld w -> pure (PWld w)
  PVar w x -> pure (PVar w x)
  PCon w c ts ps -> PCon w c <$> traverse qualType ts <*> traverse qualPatn ps

qualDecl :: Decl (Aux s) -> TQ Void s (Decl Out)
qualDecl = \case
  DType ds -> pure (DType ds)
  DDefn d -> DDefn <$> qualDefn d
  DPrim (MkPrimDecl (MkBind w x ts) s) -> do
    t1 <- case ts of
      UTUni ys0 t0 ->
        Vec.withList (toList ys0) $ \ys1 ->
          localizeTQ ys1 (\ys2 -> TUni ys2 <$> qualType t0)
      t0 -> qualType t0
    pure (DPrim (MkPrimDecl (MkBind w x t1) s))

qualModule :: Module (Aux s) -> TQ Void s (Module Out)
qualModule = (module2decls . traverse) (\top -> reset *> qualDecl top)

inferModule :: MonadError String m => Module In -> m (Module Out)
inferModule m0 = runST $ runExceptT $ do
  m1 <- runTI (inferModule' m0) m0
  runTQ (qualModule m1)
