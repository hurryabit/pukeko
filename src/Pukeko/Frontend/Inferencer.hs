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
import qualified Data.Sequence    as Seq
import qualified Data.Set         as Set
import           Data.STRef
import qualified Data.Vector.Sized as Vec

import           Pukeko.Pretty
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type       hiding ((*~>))
import           Pukeko.FrontEnd.Inferencer.UType
import           Pukeko.FrontEnd.Inferencer.Gamma
import           Pukeko.FrontEnd.Inferencer.Unify

type In    = St.KindChecker
type Out   = St.Inferencer Type
type Aux s = St.Inferencer (UType s)

type Cstrs s tv = Seq (UType s tv, Id.Clss)

data SplitCstrs s tv = MkSplitCstrs
  { _resolved   :: Map Id.TVar (Set Id.Clss)
  , _discharged :: Cstrs s tv
  }


type TI ev s = TU s ev
type IT s ev = TU s ev

runTI :: TI Void s a -> Module In -> ExceptT String (ST s) a
runTI tc module_ = runInfoT (runGammaT tc Id.freshTVars) module_

freshUVar :: TI v s (UType s tv)
freshUVar = do
  v <- fresh
  l <- getTLevel
  UVar <$> liftST (newSTRef (UFree v l))

lookupFunc :: Id.EVar -> TI v s (UType s tv)
lookupFunc x = fmap absurd . open . _sign2type <$> findInfo info2signs x

generalize :: forall s tv ev. UType s tv -> IT s ev (UType s tv, Set Id.TVar)
generalize = go
  where
    go :: UType s tv -> IT s ev (UType s tv, Set Id.TVar)
    go t0 = case t0 of
      UVar uref -> do
        uvar <- liftST (readSTRef uref)
        cur_level <- getTLevel
        case uvar of
          UFree v l
            | l > cur_level -> do
                let t1 = UTVar v
                liftST (writeSTRef uref (ULink t1))
                pure (t1, Set.singleton v)
            | otherwise     -> pureDefault
          ULink t1 -> go t1
      UTVar{} -> pureDefault
      UTArr{} -> pureDefault
      UTCon{} -> pureDefault
      UTUni{} -> bug "quantification during generalization"
      UTApp tf0 tp0 -> do
        (tf1, vsf) <- go tf0
        (tp1, vsp) <- go tp0
        pure (UTApp tf1 tp1, vsf <> vsp)
      where
        pureDefault = pure (t0, mempty)

splitCstr :: UType s tv -> Id.Clss -> TI ev s (SplitCstrs s tv)
splitCstr t0 clss = do
  (t1, tps) <- liftST (unUTApp t0)
  case t1 of
    UVar uref -> do
      uvar <- liftST (readSTRef uref)
      cur_level <- getTLevel
      case uvar of
        UFree v l
          | l > cur_level ->
              throwDoc
                ("ambiguous type variable in constraint" <+> pretty clss <+> pretty v)
          | otherwise -> pure (MkSplitCstrs mempty (Seq.singleton (t0, clss)))
        ULink{} -> bug "ULink after unUTApp"
    UTVar v -> pure (MkSplitCstrs (Map.singleton v (Set.singleton clss)) mempty)
    UTCon tcon -> do
      qual_mb <- lookupInfo info2insts (clss, tcon)
      case qual_mb of
        Nothing -> throwNoInst
        Just qual
          | length qual /= length tps ->
              bugWith "mitmatching number of type arguments for instance" (clss, tcon)
          | otherwise -> do
              let cstrs = Seq.fromList
                    [ (tp, c) | (tp, MkQVar q _) <- zip tps qual, c <- toList q ]
              splitCstrs cstrs
    UTApp{} -> bug "UTApp after unUTApp"
    UTUni{} -> bug "UTUni during constraint splitting"
    UTArr{} -> throwNoInst
  where
    throwNoInst = do
      p0 <- liftST (prettyUType prettyNormal 1 t0)
      throwDoc ("no instance for" <+> pretty clss <+> p0)


splitCstrs :: Cstrs s tv -> TI ev s (SplitCstrs s tv)
splitCstrs cstrs = fold <$> traverse (uncurry splitCstr) cstrs


instantiate ::
  Expr (Aux s) tv ev -> UType s tv ->
  IT s ev (Expr (Aux s) tv ev, UType s tv, Cstrs s tv)
instantiate e0 t0 = do
  let (qvss, t1) = unUTUni t0
  (env, cstrs) <- fmap unzip . for qvss $ \qvs -> do
    fmap unzip . for qvs $ \(MkQVar q v) -> do
      t <- freshUVar
      pure ((v, t), Seq.fromList (map ((,) t) (toList q)))
  t2 <- liftST (subst (Map.fromList (concat env)) t1)
  let e1 = foldl (\e -> mkETyApp (e^.expr2pos) e . map snd) e0 env
  pure (e1, t2, foldMap fold cstrs)

inferPatn ::
  Patn NoType tv -> UType s tv -> IT s ev (Patn (UType s) tv, Map Id.EVar (UType s tv))
inferPatn patn t_expr = case patn of
  PWld w   -> pure (PWld w, Map.empty)
  PVar w x -> pure (PVar w x, Map.singleton x t_expr)
  PCon w dcon (_ :: [NoType _]) ps0 -> do
    Some1 (Pair1
            MkTConDecl{_tcon2name = tcon, _tcon2prms = params}
            MkDConDecl{_dcon2flds = flds}) <-
      findInfo info2dcons dcon
    when (length ps0 /= length flds) $
      throwDocAt w $ "term cons" <+> quotes (pretty dcon) <+>
      "expects" <+> int (length flds) <+> "arguments"
    t_params <- traverse (const freshUVar) params
    let t_inst = appTCon tcon (toList t_params)
    unify w t_expr t_inst
    let t_fields = map (open1 . fmap (scope absurd (t_params Vec.!))) flds
    (ps1, binds) <- unzip <$> zipWithM inferPatn ps0 t_fields
    pure (PCon w dcon (toList t_params) ps1, Map.unions binds)

inferLet ::
  (BaseTVar tv, HasEnv ev) =>
  Defn In tv ev -> IT s ev (Defn (Aux s) tv ev, UType s tv, Cstrs s tv)
inferLet (MkDefn l0 r0) = do
  (r1, t1, cstrs1) <- withinTScope (infer r0)
  (t2, vs2) <- generalize t1
  MkSplitCstrs rs1 ds1 <- here (l0^.bind2pos) (splitCstrs cstrs1)
  let rs2 = Map.unionWith (<>) rs1 (Map.fromSet (const mempty) vs2)
  let qvs = map (\(v, q) -> MkQVar q v) (Map.toList rs2)
  let t3 = mkUTUni qvs t2
  pure (MkDefn (l0 & bind2type .~ t3) r1, t3, ds1)

inferRec ::
  (BaseTVar tv, HasEnv ev) =>
  Vector n (Defn In tv (EFinScope n ev)) ->
  IT s ev ( Vector n (Defn (Aux s) tv (EFinScope n ev))
          , Vector n (UType s tv)
          , Cstrs s tv
          )
inferRec defns0 = do
  let (ls0, rs0) = Vec.unzip (fmap (\(MkDefn l r) -> (l, r)) defns0)
  (rs1, ts1, cstrs1) <- withinTScope $ do
    us <- traverse (const freshUVar) ls0
    (rs1, ts1, cstrs1) <- Vec.unzip3 <$> withinEScope us (traverse infer rs0)
    Vec.zipWith3M_ (\r0 -> unify (r0^.expr2pos)) rs0 us ts1
    pure (rs1, ts1, fold cstrs1)
  (ts2, vs2) <- second fold . Vec.unzip <$> traverse generalize ts1
  MkSplitCstrs cstrs_ret1 cstrs_def <- splitCstrs cstrs1
  let cstrs_ret2 = Map.unionWith (<>) cstrs_ret1 (Map.fromSet (const mempty) vs2)
  let qvs = map (\(v, q) -> MkQVar q v) (Map.toList cstrs_ret2)
  let ts3 = fmap (mkUTUni qvs) ts2
  let vs3 = map (UTVar . _qvar2tvar) qvs
  let addETyApp w =
        scope' (EVar w . Free) (\i b -> mkETyApp w (EVar w (mkBound i b)) vs3)
  let rs2 = fmap (// addETyApp) rs1
  let defns1 = Vec.zipWith3 (\t3 -> MkDefn . (bind2type .~ t3)) ts3 ls0 rs2
  pure (defns1, ts2, cstrs_def)

infer ::
  (BaseTVar tv, HasEnv ev) =>
  Expr In tv ev -> IT s ev (Expr (Aux s) tv ev, UType s tv, Cstrs s tv)
infer = \case
    EVar w x -> lookupEVar x >>= instantiate (EVar w x)
    EVal w z -> lookupFunc z >>= instantiate (EVal w z)
    ECon w c -> typeOfDCon c >>= instantiate (ECon w c) . fmap absurd . open
    ENum w n -> pure (ENum w n, open typeInt, mempty)
    EApp w fun0 args0 -> do
      let neUnzip3 :: NonEmpty (a, b, c) -> (NonEmpty a, NonEmpty b, NonEmpty c)
          neUnzip3 ((x, y, z) :| xyzs) = (x :| xs, y :| ys, z :| zs)
            where
              (xs, ys, zs) = unzip3 xyzs
      (fun1, t_fun, cstrs_fun) <- infer fun0
      (args1, t_args, cstrs_args) <- neUnzip3 <$> traverse infer args0
      t_res <- freshUVar
      unify w t_fun (t_args *~> t_res)
      pure (EApp w fun1 args1, t_res, cstrs_fun <> fold cstrs_args)
    ELam w binds0 rhs0 NoType -> do
      t_binds <- traverse (const freshUVar) binds0
      (rhs1, t_rhs, cstrs) <- withinEScope t_binds (infer rhs0)
      let binds1 = Vec.zipWith (\(MkBind w' x NoType) -> MkBind w' x) binds0 t_binds
      pure (ELam w binds1 rhs1 t_rhs, t_binds *~> t_rhs, cstrs)
    ELet w defns0 rhs0 -> do
      (defns1, t_defns, cstrs_defns) <- Vec.unzip3 <$> traverse inferLet defns0
      (rhs1, t_rhs, cstrs_rhs) <- withinEScope t_defns (infer rhs0)
      pure (ELet w defns1 rhs1, t_rhs, fold cstrs_defns <> cstrs_rhs)
    ERec w defns0 rhs0 -> do
      (defns1, t_defns, cstrs_defns) <- inferRec defns0
      (rhs1, t_rhs, cstrs_rhs) <- withinEScope t_defns (infer rhs0)
      pure (ERec w defns1 rhs1, t_rhs, cstrs_defns <> cstrs_rhs)
    EMat w expr0 altns0 -> do
      (expr1, t_expr, cstrs0) <- infer expr0
      t_res <- freshUVar
      (altns1, cstrss1) <- fmap NE.unzip . for altns0 $ \(MkAltn w' patn0 rhs0) -> do
        (patn1, t_binds) <- inferPatn patn0 t_expr
        (rhs1, t_rhs, cstrs1) <- withinEScope t_binds (infer rhs0)
        unify w t_res t_rhs
        pure (MkAltn w' patn1 rhs1, cstrs1)
      pure (EMat w expr1 altns1, t_res, cstrs0 <> fold cstrss1)

inferTypedDefn ::
  (BaseTVar tv) =>
  Defn In tv Void -> UType s tv -> IT s Void (Defn (Aux s) tv Void)
inferTypedDefn (MkDefn (MkBind w x NoType) e0) t_decl = do
    -- NOTE: We do not instantiate the universally quantified type variables in
    -- prenex position of @t_decl@. Turning them into unification variables
    -- would make the following type check:
    --
    -- val f : a -> b
    -- let f = fun x -> x
    let (qvs, t0) = unUTUni1 t_decl
    (e1, t1, cstrs1) <- withinTScope (infer e0)
    -- NOTE: This unification should bind all unification variables in @t1@. If
    -- it does not, the quantification step will catch them.
    withQVars qvs (unify w t0 t1)
    MkSplitCstrs rs1 ds1 <- splitCstrs cstrs1
    unless (null ds1) $ bug "deferred constraints at top level"
    for_ qvs $ \(MkQVar q0 v) -> do
      let q1 = Map.findWithDefault mempty v rs1
      unless (q1 `Set.isSubsetOf` q0) $ do
        let clss = Set.findMin (q1 `Set.difference` q0)
        throwDocAt w ("cannot deduce" <+> pretty clss <+> pretty v <+> "from context")
    pure (MkDefn (MkBind w x t_decl) e1)

inferDecl :: Decl In -> TI Void s (Maybe (Decl (Aux s)))
inferDecl = \case
  DType ds -> yield (DType ds)
  DSign{} -> pure Nothing
  DClss c -> yield (DClss c)
  DInst (MkInstDecl w clss tcon (qvs :: Vector n _) ds0) -> do
    ds1 <- for ds0 $ \d0 -> do
      (_, MkSignDecl _ _ t_decl0) <- findInfo info2mthds (d0^.defn2bind.bind2evar)
      let t_inst = mkTApp (TCon tcon) (imap (\i (MkQVar _ v) -> TVar (mkBound i v)) qvs)
      let t_decl1 :: Type (TFinScope n Void)
          t_decl1 = renameType (t_decl0 >>= scope absurd (const t_inst))
      withQVars qvs (inferTypedDefn d0 (open t_decl1))
    yield (DInst (MkInstDecl w clss tcon qvs ds1))
  DDefn d0 -> do
    reset
    t_decl <- lookupFunc (d0^.defn2bind.bind2evar)
    d1 <- inferTypedDefn d0 t_decl
    yield (DDefn d1)
  DPrim (MkPrimDecl (MkBind w z NoType) s) -> do
    t <- lookupFunc z
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
  -- ys <- traverse (qvar2tvar (const fresh)) xs
  ys <- traverse (qvar2tvar pure) xs
  let env1 =
        ifoldMap
          (\i (x, y) -> Map.singleton (x^.qvar2tvar) (mkBound i (y^.qvar2tvar)))
          (Vec.zip xs ys)
  let upd env0 = env1 <> fmap Free env0
  (TQ . withReaderT upd . unTQ) (m ys)

qualType :: UType s tv' -> TQ tv s (Type tv)
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

qualDefn :: Defn (Aux s) tv' ev -> TQ tv s (Defn Out tv ev)
qualDefn (MkDefn (MkBind w x ts) e0) = case ts of
  UTUni ys0 t0 -> Vec.withList (toList ys0) $ \ys1 -> localizeTQ ys1 $ \ys2 -> do
    t1  <- TUni ys2 <$> qualType t0
    e1  <- ETyAbs w ys2 <$> qualExpr e0
    pure (MkDefn (MkBind w x t1) e1)
  t0 -> MkDefn <$> (MkBind w x <$> qualType t0) <*> qualExpr e0

qualExpr :: Expr (Aux s) tv' ev -> TQ tv s (Expr Out tv ev)
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

qualAltn :: Altn (Aux s) tv' ev -> TQ tv s (Altn Out tv ev)
qualAltn (MkAltn w p e) = MkAltn w <$> qualPatn p <*> qualExpr e

qualPatn :: Patn (UType s) tv' -> TQ tv s (Patn Type tv)
qualPatn = \case
  PWld w -> pure (PWld w)
  PVar w x -> pure (PVar w x)
  PCon w c ts ps -> PCon w c <$> traverse qualType ts <*> traverse qualPatn ps

qualDecl :: Decl (Aux s) -> TQ Void s (Decl Out)
qualDecl = \case
  DType ds -> pure (DType ds)
  DClss c -> pure (DClss c)
  -- FIXME: Do type inference for type class instances.
  DInst (MkInstDecl w c t qvs ds0) -> do
    ds1 <- for ds0 $ \d0 -> localizeTQ qvs (\_ -> qualDefn d0)
    pure (DInst (MkInstDecl w c t qvs ds1))
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

instance Monoid (SplitCstrs s tv) where
  mempty = MkSplitCstrs mempty mempty
  MkSplitCstrs rs1 ds1 `mappend` MkSplitCstrs rs2 ds2 =
    MkSplitCstrs (Map.unionWith (<>) rs1 rs2) (ds1 <> ds2)
