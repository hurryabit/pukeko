{-# LANGUAGE ViewPatterns #-}
module Pukeko.FrontEnd.Inferencer
  ( Out
  , inferModule
  )
where

import Pukeko.Prelude

import           Control.Lens.Indexed
import           Control.Monad.Freer.Supply
import           Control.Monad.ST
import qualified Data.List.NE     as NE
import qualified Data.Map         as Map
import qualified Data.Sequence    as Seq
import qualified Data.Set         as Set
import           Data.STRef
import qualified Data.Vector      as Vec

import           Pukeko.Pretty
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.SystemF    hiding (instantiate)
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type       hiding ((*~>))
import           Pukeko.FrontEnd.Inferencer.UType
import           Pukeko.FrontEnd.Inferencer.Gamma
import           Pukeko.FrontEnd.Inferencer.Unify

type In    = St.Renamer
type Out   = St.Inferencer Type
type Aux s = St.Inferencer (UType s)

type Cstrs s tv = Seq (UType s tv, Id.Clss)

data SplitCstrs s tv = MkSplitCstrs
  { _retained :: Map Id.TVar (Set Id.Clss)
  , _deferred :: Cstrs s tv
  }


type TI ev s = TU s ev
type IT s ev = TU s ev

runTI :: Module In -> TI Void s a -> Eff [Error Failure, ST s] a
runTI m0 = evalSupply sup0 . runReader noPos . runInfo m0 . runGamma
  where
    sup0 = Id.freshTVars

freshUVar :: TI v s (UType s tv)
freshUVar = do
  v <- fresh
  l <- getTLevel
  UVar <$> sendM (newSTRef (UFree v l))

generalize :: forall s tv ev. UType s tv -> IT s ev (UType s tv, Set Id.TVar)
generalize = go
  where
    go :: UType s tv -> IT s ev (UType s tv, Set Id.TVar)
    go t0 = case t0 of
      UVar uref -> do
        uvar <- sendM (readSTRef uref)
        cur_level <- getTLevel
        case uvar of
          UFree v l
            | l > cur_level -> do
                let t1 = UTVar v
                sendM (writeSTRef uref (ULink t1))
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
  (t1, tps) <- sendM (unUTApp t0)
  case t1 of
    UVar uref -> do
      uvar <- sendM (readSTRef uref)
      cur_level <- getTLevel
      case uvar of
        UFree v l
          | l > cur_level ->
              throwHere
                ("ambiguous type variable in constraint" <+> pretty clss <+> pretty v)
          | otherwise -> pure (MkSplitCstrs mempty (Seq.singleton (t0, clss)))
        ULink{} -> bug "ULink after unUTApp"
    UTVar v -> pure (MkSplitCstrs (Map.singleton v (Set.singleton clss)) mempty)
    UTCon tcon -> do
      inst_mb <- lookupInfo info2insts (clss, tcon)
      case inst_mb of
        Nothing -> throwNoInst
        Just (SomeInstDecl MkInstDecl{_inst2qvars = qvs})
          | length qual /= length tps ->
              bugWith "mitmatching number of type arguments for instance" (clss, tcon)
          | otherwise -> do
              let cstrs = Seq.fromList
                    [ (tp, c) | (tp, MkQVar q _) <- zip tps qual, c <- toList q ]
              splitCstrs cstrs
          where qual = toList qvs
    UTApp{} -> bug "UTApp after unUTApp"
    UTUni{} -> bug "UTUni during constraint splitting"
    UTArr{} -> throwNoInst
  where
    throwNoInst = do
      p0 <- sendM (prettyUType 1 t0)
      throwHere ("no instance for" <+> pretty clss <+> p0)


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
  t2 <- sendM (subst (Map.fromList (concat env)) t1)
  let e1 = foldl (\e -> mkETyApp e . map snd) e0 env
  pure (e1, t2, foldMap fold cstrs)

inferPatn ::
  Patn NoType tv -> UType s tv -> IT s ev (Patn (UType s) tv, Map Id.EVar (UType s tv))
inferPatn patn t_expr = case patn of
  PWld -> pure (PWld, Map.empty)
  PVar x -> pure (PVar x, Map.singleton x t_expr)
  PCon dcon (_ :: [NoType _]) ps0 -> do
    (MkTConDecl (unlctd -> tcon) params _dcons, MkDConDecl{_dcon2flds = flds})
      <- findInfo info2dcons dcon
    when (length ps0 /= length flds) $
      throwHere ("data constructor" <+> quotes (pretty dcon) <+>
                 "expects" <+> pretty (length flds) <+> "arguments")
    t_params <- traverse (const freshUVar) params
    let t_inst = appTCon tcon (toList t_params)
    unify t_expr t_inst
    let t_fields = map (open1 . fmap (scope absurd (Vec.fromList t_params Vec.!))) flds
    (ps1, binds) <- unzip <$> zipWithM inferPatn ps0 t_fields
    pure (PCon dcon (toList t_params) ps1, Map.unions binds)

inferLet :: (BaseTVar tv, HasEnv ev) =>
  Defn In tv ev -> IT s ev (Defn (Aux s) tv ev, UType s tv, Cstrs s tv)
inferLet (MkDefn l0 r0) = do
  (r1, t1, cstrs1) <- withinTScope (infer r0)
  (t2, vs2) <- generalize t1
  MkSplitCstrs rs1 ds1 <- splitCstrs cstrs1
  let rs2 = Map.unionWith (<>) rs1 (Map.fromSet (const mempty) vs2)
  let qvs = map (\(v, q) -> MkQVar q v) (Map.toList rs2)
  let t3 = mkUTUni qvs t2
  pure (MkDefn (l0 & bind2type .~ t3) r1, t3, ds1)

-- TODO: It would be nice to use Liquid Haskell to show that the resulting lists
-- have the same length as the input list.
inferRec :: (BaseTVar tv, HasEnv ev) =>
  [Defn In tv (EScope Int ev)] ->
  IT s ev ([Defn (Aux s) tv (EScope Int ev)], [UType s tv], Cstrs s tv)
inferRec defns0 = do
  let (ls0, rs0) = unzip (map (\(MkDefn l r) -> (l, r)) defns0)
  (rs1, ts1, cstrs1) <- withinTScope $ do
    us <- traverse (const freshUVar) ls0
    (rs1, ts1, cstrs1) <- unzip3 <$> withinEScope (Vec.fromList us) (traverse infer rs0)
    zipWithM_ unify us ts1
    pure (rs1, ts1, fold cstrs1)
  (ts2, vs2) <- second fold . unzip <$> traverse generalize ts1
  MkSplitCstrs cstrs_ret1 cstrs_def <- splitCstrs cstrs1
  let cstrs_ret2 = Map.unionWith (<>) cstrs_ret1 (Map.fromSet (const mempty) vs2)
  let qvs = map (\(v, q) -> MkQVar q v) (Map.toList cstrs_ret2)
  let ts3 = fmap (mkUTUni qvs) ts2
  let vs3 = map (UTVar . _qvar2tvar) qvs
  let addETyApp = scope' (EVar . Free) (\i b -> mkETyApp (EVar (mkBound i b)) vs3)
  let rs2 = fmap (// addETyApp) rs1
  let defns1 = zipWith3 (\t3 -> MkDefn . (bind2type .~ t3)) ts3 ls0 rs2
  pure (defns1, ts3, cstrs_def)

infer ::
  (BaseTVar tv, HasEnv ev) =>
  Expr In tv ev -> IT s ev (Expr (Aux s) tv ev, UType s tv, Cstrs s tv)
infer = \case
    ELoc (Lctd pos e0) -> here_ pos $ do
      (e1, t1, cstrs) <- infer e0
      pure (ELoc (Lctd pos e1), t1, cstrs)
    EVar x -> lookupEVar x >>= instantiate (EVar x)
    EVal z -> typeOfFunc z >>= instantiate (EVal z) . open
    ECon c -> typeOfDCon c >>= instantiate (ECon c) . open
    ENum n -> pure (ENum n, open typeInt, mempty)
    EApp fun0 args0 -> do
      let neUnzip3 :: NonEmpty (a, b, c) -> (NonEmpty a, NonEmpty b, NonEmpty c)
          neUnzip3 ((x, y, z) :| xyzs) = (x :| xs, y :| ys, z :| zs)
            where
              (xs, ys, zs) = unzip3 xyzs
      (fun1, t_fun, cstrs_fun) <- infer fun0
      (args1, t_args, cstrs_args) <- neUnzip3 <$> traverse infer args0
      t_res <- freshUVar
      unify t_fun (t_args *~> t_res)
      pure (EApp fun1 args1, t_res, cstrs_fun <> fold cstrs_args)
    ELam binds0 rhs0 NoType -> do
      t_binds <- traverse (const freshUVar) binds0
      (rhs1, t_rhs, cstrs) <- withinEScope (NE.toVector t_binds) (infer rhs0)
      let binds1 = NE.zipWith (\(MkBind x NoType) -> MkBind x) binds0 t_binds
      pure (ELam binds1 rhs1 t_rhs, t_binds *~> t_rhs, cstrs)
    ELet defns0 rhs0 -> do
      (defns1, t_defns, cstrs_defns) <- unzip3 <$> traverse inferLet defns0
      (rhs1, t_rhs, cstrs_rhs) <- withinEScope (Vec.fromList t_defns) (infer rhs0)
      pure (ELet defns1 rhs1, t_rhs, fold cstrs_defns <> cstrs_rhs)
    ERec defns0 rhs0 -> do
      (defns1, t_defns, cstrs_defns) <- inferRec defns0
      (rhs1, t_rhs, cstrs_rhs) <- withinEScope (Vec.fromList t_defns) (infer rhs0)
      pure (ERec defns1 rhs1, t_rhs, cstrs_defns <> cstrs_rhs)
    EMat expr0 altns0 -> do
      (expr1, t_expr, cstrs0) <- infer expr0
      t_res <- freshUVar
      (altns1, cstrss1) <- fmap NE.unzip . for altns0 $ \(MkAltn patn0 rhs0) -> do
        (patn1, t_binds) <- inferPatn patn0 t_expr
        (rhs1, t_rhs, cstrs1) <- withinEScope t_binds (infer rhs0)
        unify t_res t_rhs
        pure (MkAltn patn1 rhs1, cstrs1)
      pure (EMat expr1 altns1, t_res, cstrs0 <> fold cstrss1)
    ECoe c@(MkCoercion dir tcon NoType NoType) e0 -> do
      MkTConDecl _ prms dcons <- findInfo info2tcons tcon
      t_rhs0 <- case dcons of
        Right _ -> throwHere ("type constructor" <+> pretty tcon <+> "is not coercible")
        Left t_rhs0 -> pure t_rhs0
      t_prms <- traverse (const freshUVar) prms
      let t_lhs = appTCon tcon (toList t_prms)
      let t_rhs = open1 (fmap (scope absurd (Vec.fromList t_prms Vec.!)) t_rhs0)
      let (t_to, t_from) = case dir of
            Inject  -> (t_lhs, t_rhs)
            Project -> (t_rhs, t_lhs)
      (e1, t1, cstrs) <- infer e0
      unify t_from t1
      pure (ECoe c{_coeFrom = t_from, _coeTo = t_to} e1, t_to, cstrs)

inferTypedDefn ::
  (BaseTVar tv) =>
  Defn In tv Void -> UType s tv -> IT s Void (Defn (Aux s) tv Void)
inferTypedDefn (MkDefn (MkBind x NoType) e0) t_decl = do
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
    withQVars qvs (unify t0 t1)
    MkSplitCstrs rs1 ds1 <- splitCstrs cstrs1
    unless (null ds1) $ bug "deferred constraints at top level"
    for_ qvs $ \(MkQVar q0 v) -> do
      let q1 = Map.findWithDefault mempty v rs1
      unless (q1 `Set.isSubsetOf` q0) $ do
        let clss = Set.findMin (q1 `Set.difference` q0)
        throwHere ("cannot deduce" <+> pretty clss <+> pretty v <+> "from context")
    pure (MkDefn (MkBind x t_decl) e1)

inferDecl :: Decl In -> TI Void s (Maybe (Decl (Aux s)))
inferDecl = here' $ \case
  DType ds -> yield (DType ds)
  DSign{} -> pure Nothing
  DClss c -> yield (DClss c)
  DInst (MkInstDecl clss tcon qvs ds0) -> do
    ds1 <- for ds0 $ \d0 -> do
      (_, MkSignDecl _ t_decl0) <-
        findInfo info2mthds (d0^.defn2bind.bind2evar.lctd)
      let t_inst = mkTApp (TCon tcon) (imap (\i (MkQVar _ v) -> TVar (mkBound i v)) qvs)
      let t_decl1 :: Type (TScope Int Void)
          t_decl1 = renameType (instantiate' (const t_inst) t_decl0)
      withQVars qvs (inferTypedDefn d0 (open t_decl1))
    yield (DInst (MkInstDecl clss tcon qvs ds1))
  DDefn d0 -> do
    reset @Id.TVar
    t_decl <- open <$> typeOfFunc (d0^.defn2bind.bind2evar.lctd)
    d1 <- inferTypedDefn d0 t_decl
    yield (DDefn d1)
  DExtn (MkExtnDecl (MkBind z NoType) s) -> do
    t <- open <$> typeOfFunc (unlctd z)
    yield (DExtn (MkExtnDecl (MkBind z t) s))
  where
    yield = pure . Just

inferModule' :: Module In -> TI Void s (Module (Aux s))
inferModule' = module2decls (fmap catMaybes . traverse inferDecl)

type TQEnv tv = Map Id.TVar tv

type TQ tv s =
  Eff [Reader (TQEnv tv), Reader SourcePos, Supply Id.TVar, Error Failure, ST s]

runTQ :: TQ Void s a -> Eff [Error Failure, ST s] a
runTQ = evalSupply sup0 . runReader noPos . runReader env0
  where
    env0 = mempty
    sup0 = map (Id.tvar . (:[])) ['a' .. 'z'] ++ Id.freshTVars

localizeTQ :: (TraversableWithIndex Int t) =>
  t QVar -> TQ (TScope Int tv) s a -> TQ tv s a
localizeTQ qvs =
  let env1 = ifoldMap (\i (MkQVar _ v) -> Map.singleton v (mkBound i v)) qvs
      upd env0 = env1 <> fmap Free env0
  in  local' upd

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
    uvar <- sendM (readSTRef uref)
    case uvar of
      UFree x _ -> bugWith "free unification variable during quantification" x
      ULink t -> qualType t

qualDefn :: Defn (Aux s) tv' ev -> TQ tv s (Defn Out tv ev)
qualDefn (MkDefn (MkBind x t0) e0) = case t0 of
  UTUni (toList -> qvs) t1 -> localizeTQ qvs $ do
    t2  <- mkTUni qvs <$> qualType t1
    e2  <- mkETyAbs qvs <$> qualExpr e0
    pure (MkDefn (MkBind x t2) e2)
  _ -> MkDefn <$> (MkBind x <$> qualType t0) <*> qualExpr e0

qualExpr :: Expr (Aux s) tv' ev -> TQ tv s (Expr Out tv ev)
qualExpr = \case
  ELoc le -> here le $ ELoc <$> lctd qualExpr le
  EVar x -> pure (EVar x)
  EVal z -> pure (EVal z)
  ECon c -> pure (ECon c)
  ENum n -> pure (ENum n)
  EApp e0 es -> EApp <$> qualExpr e0 <*> traverse qualExpr es
  ELam bs0 e0 t -> do
    bs1 <- for bs0 $ \(MkBind x ts) -> do
      MkBind x <$> qualType ts
    ELam bs1 <$> qualExpr e0 <*> qualType t
  ELet ds e0 -> ELet <$> traverse qualDefn ds <*> qualExpr e0
  ERec ds e0 -> ERec <$> traverse qualDefn ds <*> qualExpr e0
  EMat e0 as -> EMat <$> qualExpr e0 <*> traverse qualAltn as
  ECoe c  e0 -> ECoe <$> coercion2type qualType c <*> qualExpr e0
  ETyAbs _ _ -> bug "type abstraction during quantification"
  ETyApp e0 ts -> ETyApp <$> qualExpr e0 <*> traverse qualType ts

qualAltn :: Altn (Aux s) tv' ev -> TQ tv s (Altn Out tv ev)
qualAltn (MkAltn p e) = MkAltn <$> qualPatn p <*> qualExpr e

qualPatn :: Patn (UType s) tv' -> TQ tv s (Patn Type tv)
qualPatn = \case
  PWld -> pure PWld
  PVar x -> pure (PVar x)
  PCon c ts ps -> PCon c <$> traverse qualType ts <*> traverse qualPatn ps

qualDecl :: Decl (Aux s) -> TQ Void s (Decl Out)
qualDecl = \case
  DType ds -> pure (DType ds)
  DClss c -> pure (DClss c)
  -- FIXME: Do type inference for type class instances.
  DInst (MkInstDecl c t qvs ds0) -> do
    ds1 <- for ds0 $ \d0 -> localizeTQ qvs (qualDefn d0)
    pure (DInst (MkInstDecl c t qvs ds1))
  DDefn d -> DDefn <$> qualDefn d
  DExtn (MkExtnDecl (MkBind x ts) s) -> do
    t1 <- case ts of
      UTUni (toList -> qvs) t0 -> localizeTQ qvs (mkTUni qvs <$> qualType t0)
      t0 -> qualType t0
    pure (DExtn (MkExtnDecl (MkBind x t1) s))

qualModule :: Module (Aux s) -> TQ Void s (Module Out)
qualModule = module2decls (traverse (\decl -> reset @Id.TVar *> qualDecl decl))

inferModule :: Module In -> Either Failure (Module Out)
inferModule m0 = runST $ runM . runError $ do
  m1 <- runTI m0 (inferModule' m0)
  runTQ (qualModule m1)

instance Monoid (SplitCstrs s tv) where
  mempty = MkSplitCstrs mempty mempty
  MkSplitCstrs rs1 ds1 `mappend` MkSplitCstrs rs2 ds2 =
    MkSplitCstrs (Map.unionWith (<>) rs1 rs2) (ds1 <> ds2)
