{-# LANGUAGE TypeApplications #-}
module Pukeko.Language.Inferencer
  ( Out
  , inferModule
  )
where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Supply
import           Control.Monad.Writer
import           Control.Monad.ST
import           Control.Monad.ST.Class
import           Data.Foldable
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Data.STRef
import           Data.Traversable
import qualified Data.Vector.Sized as Vec

import           Pukeko.Error
import           Pukeko.Pos
import           Pukeko.Pretty
import           Pukeko.Language.Info
import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage         as St
import qualified Pukeko.Language.AST.ConDecl       as Con
import qualified Pukeko.Language.AST.ModuleInfo    as MI
import qualified Pukeko.Language.AST.Scope         as Sc
import qualified Pukeko.Language.Ident             as Id
import           Pukeko.Language.Type              (NoType (..), Type (..), typeInt)
import           Pukeko.Language.Inferencer.UType
import qualified Pukeko.Language.Inferencer.Unify as U

type In    = St.KindChecker
type Out   = St.Inferencer Type
type Aux s = St.Inferencer (UType s)

data Environment v s = MkEnvironment
  { _locals :: Sc.EnvOf v (UType s Void)
  , _level  :: Int
  }
makeLenses ''Environment

newtype TI v s a =
  TI{unTI :: InfoT (ModuleInfo In) (ReaderT (Environment v s) (SupplyT Id.TVar (ExceptT String (ST s)))) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (Environment v s)
           , MonadSupply Id.TVar
           , MonadInfo (GenModuleInfo 'True 'True)
           )

instance MonadST (TI v s) where
  type World (TI v s) = s
  liftST = TI . liftST

runTI :: TI Id.EVar s a -> ModuleInfo In -> ExceptT String (ST s) a
runTI tc info = do
  let locals0 = fmap (open . snd) (MI.funs info)
  let env0 = MkEnvironment locals0 0
  evalSupplyT (runReaderT (runInfoT (unTI tc) info) env0) Id.freshTVars

freshUVar :: TI v s (UType s Void)
freshUVar = do
  x <- fresh
  l <- view level
  UVar <$> liftST (newSTRef (UFree x l))

unify :: Pos -> UType s Void -> UType s Void -> TI v s ()
unify w t1 t2 = TI $ lift $ lift $ lift $ U.unify w t1 t2

localize ::
  forall i v s a.
  (IsVarLevel i, IsVar v) =>
  EnvLevelOf i (UType s Void) ->
  TI (EScope i v) s a ->
  TI v s a
localize ts = TI . mapInfoT (withReaderT (locals %~ Sc.extendEnv @i @v ts)) . unTI

lookupType :: (IsVar v) => v -> TI v s (UType s Void)
lookupType = views locals . Sc.lookupEnv

generalize :: UType s Void -> TI v s (UType s Void, [UType s Void])
generalize t0 = do
  (t1, xs1) <- runWriterT (go t0)
  let xs2 = toList xs1
  pure (mkUTUni xs2 t1, map UTVar xs2)
  where
    go t0 = case t0 of
      (UVar uref) -> do
        uvar <- lift (liftST (readSTRef uref))
        cur_level <- view level
        case uvar of
          UFree x l
            | l > cur_level -> do
                let t1 = UTVar x
                lift (liftST (writeSTRef uref (ULink t1)))
                tell (Set.singleton x)
                pure t1
            | otherwise     -> pure t0
          ULink t1 -> go t1
      UTVar{} -> pure t0
      UTArr{} -> pure t0
      UTCon{} -> pure t0
      UTUni{} -> bug "inferencer" "quantification during generalization" Nothing
      UTApp tf tp -> UTApp <$> go tf <*> go tp

instantiate :: UType s Void -> TI v s (UType s Void, [UType s Void])
instantiate ts = do
  let (xs, t0) = unUTUni ts
  uvars <- traverse (const freshUVar) xs
  let env = Map.fromList (zip xs uvars)
  t1 <- liftST (subst env t0)
  pure (t1, uvars)

instantiateTCon :: Id.TCon -> TI v s (UType s Void, [UType s Void], Map.Map Id.TVar (UType s Void))
instantiateTCon tcon = do
  Con.MkTConDecl{_params} <- findTCon tcon
  let params = toList _params
  t_params <- traverse (const freshUVar) params
  return (appTCon tcon t_params, t_params, Map.fromList (zip params t_params))

inferPatn ::
  Patn In Void -> UType s Void -> TI v s (Patn (Aux s) Void, Map.Map Id.EVar (UType s Void))
inferPatn patn t_expr = case patn of
  PWld w   -> pure (PWld w, Map.empty)
  PVar w x -> pure (PVar w x, Map.singleton x t_expr)
  PCon w dcon (_ :: [NoType Void]) ps0 -> do
    Con.MkDConDecl Con.MkDConDeclN{_dname, _tcon, _fields} <- findDCon dcon
    when (length ps0 /= length _fields) $
      throwDocAt w $ "term cons" <+> quotes (pretty _dname) <+>
      "expects" <+> int (length _fields) <+> "arguments"
    (t_inst, t_params, env_inst) <- instantiateTCon _tcon
    unify w t_expr t_inst
    -- TODO: Remove this @fmap baseName@ hack.
    t_fields <- liftST $ traverse (subst env_inst . open1 . fmap baseName) _fields
    (ps1, binds) <- unzip <$> zipWithM inferPatn ps0 t_fields
    pure (PCon w dcon t_params ps1, Map.unions binds)

-- TODO: Share mode core between 'inferLet' and 'inferRec'
-- TODO: Add test to ensure types are generalized properly.
inferLet ::
  (IsEVar v) =>
  Vec.Vector n (Defn In Void v) ->
  TI v s (Vec.Vector n (Defn (Aux s) Void v), Vec.Vector n (UType s Void))
inferLet defns = do
  -- TODO: Make this less imperative.
  (rhss, t_rhss) <- local (level +~ 1) $ do
    t_lhss <- traverse (const freshUVar) defns
    (rhss, t_rhss) <- Vec.unzip <$> traverse (infer . view rhs) defns
    ifor_ defns $ \i defn ->
      unify (defn^.pos) (t_lhss Vec.! i) (t_rhss Vec.! i)
    pure (rhss, t_rhss)
  fmap Vec.unzip $ ifor defns $ \i (MkDefn (MkBind w x NoType) _) -> do
    let rhs = rhss Vec.! i
    (ts_rhs, _) <- generalize (t_rhss Vec.! i)
    pure (MkDefn (MkBind w x ts_rhs) rhs, ts_rhs)

inferRec ::
  (IsEVar v) =>
  Vec.Vector n (Defn In Void (EFinScope n v)) ->
  TI v s ( Vec.Vector n (Defn (Aux s) Void (EFinScope n v))
         , Vec.Vector n (UType s Void))
inferRec defns0 = do
  let (ls0, rs0) = Vec.unzip (fmap (\(MkDefn l r) -> (l, r)) defns0)
  (rs1, ts1) <- local (level +~ 1) $ do
    us <- traverse (const freshUVar) ls0
    (rs1, ts1) <- Vec.unzip <$> localize us (traverse infer rs0)
    for_ (Vec.zip3 ls0 us ts1) $ \(l0, u, t1) -> unify (l0^.pos) u t1
    pure (rs1, ts1)
  (ts2, qs) <- Vec.unzip <$> traverse generalize ts1
  let ls1 = Vec.zipWith (set bindType) ts2 ls0
  let f w =
        scope'
        (\i b -> mkETyApp w (EVar w (mkBound i b)) (map (fmap absurd) (qs Vec.! i)))
        (EVar w . Free)
  let rs2 = fmap (// f) rs1
  let defns1 = Vec.zipWith MkDefn ls1 rs2
  pure (defns1, ts2)

mkETyApp1 :: Expr (Aux s) Void v -> [UType s Void] -> Expr (Aux s) Void v
mkETyApp1 e0 ys
  | null ys = e0
  | otherwise = ETyApp (e0^.pos) e0 ys


infer :: IsEVar v => Expr In Void v -> TI v s (Expr (Aux s) Void v, UType s Void)
infer = \case
    EVar w x -> do
      (t, us) <- lookupType x >>= instantiate
      pure (mkETyApp1 (EVar w x) us, t)
    ECon w c -> do
      t0 <- typeOfDCon c
      (t1, us) <- instantiate (open t0)
      pure (mkETyApp1 (ECon w c) us, t1)
    ENum w n -> pure (ENum w n, open typeInt)
    EApp w fun0 args0 -> do
      (fun1, t_fun) <- infer fun0
      (args1, t_args) <- unzip <$> traverse infer args0
      t_res <- freshUVar
      unify w t_fun (t_args *~> t_res)
      pure (EApp w fun1 args1, t_res)
    ELam w binds0 rhs0 -> do
      t_binds <- traverse (const freshUVar) binds0
      (rhs1, t_rhs) <- localize t_binds (infer rhs0)
      let binds1 = Vec.zipWith (\(MkBind w' x NoType) -> MkBind w' x) binds0 t_binds
      pure (ELam w binds1 rhs1, toList t_binds *~> t_rhs)
    ELet w defns0 rhs0 -> do
      (defns1, t_defns) <- inferLet defns0
      (rhs1, t_rhs) <- localize t_defns (infer rhs0)
      pure (ELet w defns1 rhs1, t_rhs)
    ERec w defns0 rhs0 -> do
      (defns1, t_defns) <- inferRec defns0
      (rhs1, t_rhs) <- localize t_defns (infer rhs0)
      pure (ERec w defns1 rhs1, t_rhs)
    EMat w expr0 altns0 -> do
      (expr1, t_expr) <- infer expr0
      t_res <- freshUVar
      altns1 <- for altns0 $ \(MkAltn w' patn0 rhs0) -> do
        (patn1, t_binds) <- inferPatn patn0 t_expr
        (rhs1, t_rhs) <- localize t_binds (infer rhs0)
        unify w t_res t_rhs
        pure (MkAltn w' patn1 rhs1)
      pure (EMat w expr1 altns1, t_res)

inferTopLevel :: TopLevel In -> TI Id.EVar s [TopLevel (Aux s)]
inferTopLevel = \case
  TLLet _ defns -> handleLetOrRec inferLet  id            defns
  TLRec _ defns -> handleLetOrRec inferRec (fmap unscope) defns
  TLAsm (MkBind w x NoType) asm -> do
    ts <- lookupType x
    pure [TLAsm (MkBind w x ts) asm]
  where
    handleLetOrRec inferLetOrRec squashScope defns0 = do
      reset
      (defns1, _) <- inferLetOrRec defns0
      for (toList defns1) $ \(MkDefn b@(MkBind w x ts_defn) e) -> do
        (t_defn, _us) <- instantiate ts_defn
        -- NOTE: If we instantiate the type schema, universally quantified type
        -- variables would be turned into unification variables and the
        -- following would type check:
        --
        -- val f : a -> b
        -- let f = fun x -> x
        (_, t_decl) <- unUTUni <$> lookupType x
        unify w t_decl t_defn
        pure (TLDef b (squashScope e))

inferModule' :: Module In -> TI Id.EVar s (Module (Aux s))
inferModule' (MkModule decls tops)=
  MkModule decls <$> concat <$> traverse inferTopLevel tops

type TQEnv tv = Map.Map Id.TVar tv

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
  Vec.Vector n Id.TVar ->
  (Vec.Vector n Id.TVar -> TQ (TFinScope n tv) s a) ->
  TQ tv s a
localizeTQ xs m = do
  ys <- traverse (const fresh) xs
  let env1 = ifoldMap (\i (x, y) -> Map.singleton x (mkBound i y)) (Vec.zip xs ys)
  let upd env0 = env1 <> fmap Free env0
  (TQ . withReaderT upd . unTQ) (m ys)

qualType :: UType s Void -> TQ tv s (Type tv)
qualType = \case
  UTVar x -> do
    y_mb <- asks (x `Map.lookup`)
    case y_mb of
      Nothing -> bug "type checker"
                 "unknown type variable during quantification" (Just (show x))
      Just y  -> pure (TVar y)
  UTArr -> pure TArr
  UTCon c -> pure (TCon c)
  UTApp t1 t2 -> TApp <$> qualType t1 <*> qualType t2
  UTUni{} -> bug "inferencer" "quantification during quantification" Nothing
  UVar uref -> do
    uvar <- liftST (readSTRef uref)
    case uvar of
      UFree x _ -> bug "type checker"
                   "free unification variable during quantification" (Just (show x))
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
  ECon w c -> pure (ECon w c)
  ENum w n -> pure (ENum w n)
  EApp w e0 es -> EApp w <$> qualExpr e0 <*> traverse qualExpr es
  ELam w bs0 e0 -> do
    bs1 <- for bs0 $ \(MkBind w' x ts) -> do
      MkBind w' x <$> qualType ts
    ELam w bs1 <$> qualExpr e0
  ELet w ds e0 -> ELet w <$> traverse qualDefn ds <*> qualExpr e0
  ERec w ds e0 -> ERec w <$> traverse qualDefn ds <*> qualExpr e0
  EMat w e0 as -> EMat w <$> qualExpr e0 <*> traverse qualAltn as
  ETyAbs _ _ _ -> bug "type checker" "type abstraction during quantification" Nothing
  ETyApp w e0 ts -> ETyApp w <$> qualExpr e0 <*> traverse qualType ts

qualAltn :: Altn (Aux s) Void ev -> TQ tv s (Altn Out tv ev)
qualAltn (MkAltn w p e) = MkAltn w <$> qualPatn p <*> qualExpr e

qualPatn :: Patn (Aux s) Void -> TQ tv s (Patn Out tv)
qualPatn = \case
  PWld w -> pure (PWld w)
  PVar w x -> pure (PVar w x)
  PCon w c ts ps -> PCon w c <$> traverse qualType ts <*> traverse qualPatn ps

qualTopLevel :: TopLevel (Aux s) -> TQ Void s (TopLevel Out)
qualTopLevel = \case
  TLDef b e -> do
    MkDefn b e <- qualDefn (MkDefn b e)
    pure (TLDef b e)
  TLAsm (MkBind w x ts) s -> do
    -- TODO: Avoid code duplication with qualDefn.
    t1 <- case ts of
      UTUni ys0 t0 ->
        Vec.withList (toList ys0) $ \ys1 ->
          localizeTQ ys1 (\ys2 -> TUni ys2 <$> qualType t0)
      t0 -> qualType t0
    pure (TLAsm (MkBind w x t1) s)

qualModule :: Module (Aux s) -> TQ Void s (Module Out)
qualModule = (module2tops . traverse) (\top -> reset *> qualTopLevel top)

inferModule :: MonadError String m => Module In -> m (Module Out)
inferModule m0@(MkModule info0 _) = runST $ runExceptT $ do
  m1 <- runTI (inferModule' m0) info0
  runTQ (qualModule m1)
