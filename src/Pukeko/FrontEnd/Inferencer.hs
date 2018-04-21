module Pukeko.FrontEnd.Inferencer
  ( Out
  , inferModule
  )
where

import Pukeko.Prelude
import Pukeko.Pretty

-- import           Control.Lens (folded)
import           Control.Monad.Extra
import           Control.Monad.Freer.Supply
import qualified Data.List.NE as NE
import qualified Data.Map.Extended as Map
import qualified Data.Set as Set
import           Data.STRef

import Pukeko.AST.ConDecl
import Pukeko.AST.Dict
import Pukeko.AST.Expr.Optics
import Pukeko.AST.Language
import Pukeko.AST.Name
import Pukeko.AST.SystemF
import Pukeko.AST.Type hiding ((~>))
import Pukeko.FrontEnd.Inferencer.Constraints
import Pukeko.FrontEnd.Inferencer.Gamma
import Pukeko.FrontEnd.Inferencer.Unify
import Pukeko.FrontEnd.Inferencer.UType
import Pukeko.FrontEnd.Info

type In    = Surface
type Out   = Typed
type Aux s = PreTyped (UType s) (MDict s)

type CanInfer s effs = (CanUnify s effs, CanSolve s effs, Member (Supply UVarId) effs)

freshUVar :: forall s effs. CanInfer s effs => Eff effs (UType s)
freshUVar = do
  v <- fresh
  l <- getLevel @s
  UVar <$> sendM (newSTRef (UFree v l))

generalize :: forall s effs. CanInfer s effs =>
  UType s -> Eff effs (UType s, Set TyVar)
generalize = go
  where
    go :: UType s -> Eff effs (UType s, Set TyVar)
    go t0 = case t0 of
      UVar uref -> do
        curLevel <- getLevel @s
        sendM (readSTRef uref) >>= \case
          UFree uid lvl
            | lvl > curLevel -> do
                name <- mkName (Lctd noPos (uvarIdName uid))
                let t1 = UTVar name
                sendM (writeSTRef uref (ULink t1))
                pure (t1, Set.singleton name)
            | otherwise -> pureDefault
          ULink t1 -> go t1
      UTVar{} -> pureDefault
      UTAtm{} -> pureDefault
      UTUni{} -> impossible  -- we have only rank-1 types
      UTCtx{} -> impossible
      UTApp tf0 tp0 -> do
        (tf1, vsf) <- go tf0
        (tp1, vsp) <- go tp0
        pure (UTApp tf1 tp1, vsf <> vsp)
      where
        pureDefault = pure (t0, mempty)

instantiate :: CanInfer s effs =>
  Expr (Aux s) -> UType s -> Eff effs (Expr (Aux s), UType s, UnsolvedCstrs s)
instantiate = go Map.empty mempty
  where
    go env0 cstrs0 e0 = \case
      UTUni v t0 -> do
        tv <- freshUVar
        go (Map.insert v tv env0) cstrs0 (ETyApp e0 tv) t0
      UTCtx (clss, tc0) t0 -> do
        tc1 <- sendM (substUType env0 tc0)
        (dict, cstr1) <- freshConstraint (clss, tc1)
        go env0 (cstrs0 <> cstr1) (ECxApp e0 dict) t0
      t0 -> do
        t1 <- sendM (substUType env0 t0)
        pure (e0, t1, cstrs0)

-- TODO: Try removing the returned map.
inferPatn :: CanInfer s effs => Patn In -> UType s -> Eff effs (Patn (Aux s))
inferPatn patn t_expr = case patn of
  PWld -> pure PWld
  PVar (x, NoType) -> pure (PVar (x, t_expr))
  PCon dcon ps0 -> do
    (MkTyConDecl tcon params _dcons, MkTmConDecl{_tmcon2fields = fields})
      <- findInfo info2tmcons dcon
    when (length ps0 /= length fields) $
      throwHere ("data constructor" <+> quotes (pretty dcon) <+>
                 "expects" <+> pretty (length fields) <+> "arguments")
    t_params <- traverse (const freshUVar) params
    let t_inst = rewindl UTApp (UTCon tcon) t_params
    unify t_expr t_inst
    let env = Map.fromList (zipExact params t_params)
    let t_fields = map (open1 . fmap (env Map.!)) fields
    ps1 <- zipWithM inferPatn ps0 t_fields
    pure (PCon dcon ps1)

addTyCxAbs
  :: [TyVar]
  -> [DxBinder (UType s)]
  -> Expr (Aux s)
  -> UType s
  -> (Expr (Aux s), UType s)
addTyCxAbs tyVars dxBinders expr typ
  | null tyVars && null dxBinders = (expr, typ)
  | otherwise =
      ( rewindr ETyAbs tyVars $ rewindr ECxAbs         dxBinders  $ ETyAnn typ expr
      , rewindr UTUni  tyVars $ rewindr UTCtx (map snd dxBinders)          typ
      )

inferLet :: forall s effs.
  CanInfer s effs => Bind In -> Eff effs (Bind (Aux s), UnsolvedCstrs s)
inferLet (MkBind (tmVar, NoType) rhs0) = do
  (rhs1, t'_rhs1, cstrs) <- enterLevel @s (infer rhs0)
  (t_rhs1, toList -> tyVars) <- generalize t'_rhs1
  (rets, defs) <- solveConstraints cstrs
  let (rhs2, t_rhs2) = addTyCxAbs tyVars rets rhs1 t_rhs1
  pure (MkBind (tmVar, t_rhs2) rhs2, defs)

-- TODO: It would be nice to use Liquid Haskell to show that the resulting lists
-- have the same length as the input list.
inferRec :: forall s effs. CanInfer s effs =>
  [Bind In] -> Eff effs ([Bind (Aux s)], UnsolvedCstrs s)
inferRec binds0 = do
  let (binders0, rhss0) = unzip (map (\(MkBind (b, NoType) r) -> (b, r)) binds0)
  (rhss1, t'_rhss1, cstrs1) <- enterLevel @s $ do
    binders1 <- for binders0 $ \b -> (,) b <$> freshUVar
    (rhss1, t'_rhss1, cstrs1) <- unzip3 <$> introTmVars binders1 (traverse infer rhss0)
    zipWithM_ (unify . snd) binders1 t'_rhss1
    pure (rhss1, t'_rhss1, fold cstrs1)
  (t_rhss1, toList . fold -> tyVars) <-  unzip <$> traverse generalize t'_rhss1
  (rets1, defs1) <- solveConstraints cstrs1
  let dicts = map (MDVar . fst) rets1
  let bound = Set.fromList binders0
  let addApps x
        | x `Set.member` bound =
            foldl ECxApp (foldl ETyApp (EVar x) (map UTVar tyVars)) dicts
        | otherwise = EVar x
  let binds2 =
        zipWith3
          (\binder0 rhs1 t_rhs1 ->
             let (rhs2, t_rhs2) =
                   addTyCxAbs tyVars rets1 (substitute addApps rhs1) t_rhs1
              in  MkBind (binder0, t_rhs2) rhs2)
          binders0
          rhss1
          t_rhss1
  pure (binds2, defs1)

infer :: forall s effs. CanInfer s effs =>
  Expr In -> Eff effs (Expr (Aux s), UType s, UnsolvedCstrs s)
infer = \case
    ELoc (Lctd pos e0) -> here_ pos $ do
      (e1, t1, cstrs) <- infer e0
      pure (ELoc (Lctd pos e1), t1, cstrs)
    EVar x -> lookupTmVar x >>= instantiate (EVar x)
    EAtm a -> typeOfAtom a >>= instantiate (EAtm a) . open
    EApp fun0 (TmArg arg0) -> do
      (fun1, t_fun, cstrs_fun) <- infer fun0
      (arg1, t_arg, cstrs_arg) <- infer arg0
      t_res <- freshUVar
      unify t_fun (t_arg ~> t_res)
      pure (ETmApp fun1 arg1, t_res, cstrs_fun <> cstrs_arg)
    EAbs (TmPar (param, NoType)) body0 -> do
      t_param <- freshUVar
      let binder = (param, t_param)
      (body1, t_body, cstrs) <- introTmVar @s binder (infer body0)
      pure (ETmAbs binder (ETyAnn t_body body1), t_param ~> t_body, cstrs)
    ELet mode binds0 body0 -> do
      (binds1, cstrs_binds) <- case mode of
        BindPar -> second fold . unzip <$> traverse inferLet binds0
        BindRec -> inferRec binds0
      (body1, t_body1, cstrs_body) <- introTmVars (map _b2binder binds1) $ infer body0
      pure (ELet mode binds1 body1, t_body1, cstrs_binds <> cstrs_body)
    EMat NoType expr0 altns0 -> do
      (expr1, t_expr, cstrs0) <- infer expr0
      t_res <- freshUVar
      (altns1, cstrss1) <- fmap NE.unzip . for altns0 $ \(MkAltn patn0 rhs0) -> do
        patn1 <- inferPatn patn0 t_expr
        let binders = toListOf patn2binder patn1
        (rhs1, t_rhs, cstrs1) <- introTmVars binders (infer rhs0)
        unify t_res t_rhs
        pure (MkAltn patn1 rhs1, cstrs1)
      pure (EMat t_expr expr1 altns1, t_res, cstrs0 <> fold cstrss1)
    ECast (c@(MkCoercion dir tcon), NoType) e0 -> do
      MkTyConDecl _ prms dcons <- findInfo info2tycons tcon
      t_rhs0 <- case dcons of
        Right _ -> throwHere ("type constructor" <+> pretty tcon <+> "is not coercible")
        Left t_rhs0 -> pure t_rhs0
      t_prms <- traverse (const freshUVar) prms
      let t_lhs = rewindl UTApp (UTCon tcon) t_prms
      let env = Map.fromList (zipExact prms t_prms)
      let t_rhs = open1 (fmap (env Map.!) t_rhs0)
      let (t_to, t_from) = case dir of
            Inject  -> (t_lhs, t_rhs)
            Project -> (t_rhs, t_lhs)
      (e1, t1, cstrs) <- infer e0
      unify t_from t1
      pure (ECast (c, t_to) e1, t_to, cstrs)

inferFuncDecl :: forall s tv effs. CanInfer s effs =>
  FuncDecl In tv -> UType s -> Eff effs (FuncDecl (Aux s) tv)
inferFuncDecl (MkFuncDecl name NoType body0) t_decl = do
    -- NOTE: We do not instantiate the universally quantified type variables in
    -- prenex position of @t_decl@. Turning them into unification variables
    -- would make the following type check:
    --
    -- f : a -> b
    -- f = fun x -> x
    let (vs0, (ctxt0, t_body0)) = second (unwindr _UTCtx) (unwindr _UTUni t_decl)
    ctxt1 <- for ctxt0 $ \case
      cstr@(clss, UTVar tvar) -> do
        dvar <- mkDxVar clss tvar
        pure (dvar, cstr)
      _ -> impossible
    (body1, t_body1, cstrs1) <- enterLevel @s $ infer body0
    (rets1, defs1) <- introTyVars @s vs0 $ enterContext ctxt1 $ do
      -- NOTE: This unification should bind all unification variables in @t1@. If
      -- it does not, the freezing will catch this (and error out).
      unify t_body0 t_body1
      solveConstraints cstrs1
    assertM (null rets1 && null defs1)
    let (body2, _) = addTyCxAbs vs0 ctxt1 body1 t_body1
    pure (MkFuncDecl name t_decl body2)

inferInstDecl
  :: forall s effs. CanInfer s effs
  => InstDecl In -> Eff effs (InstDecl (Aux s))
inferInstDecl (MkInstDecl instName clss atom prms ctxt0 super0 ds0) = do
    clssDecl <- findInfo info2classes clss
    let ctxt1 = map (second (second open)) ctxt0
    introTyVars @s prms $ enterContext @s ctxt1 $ do
      let t_inst = foldl TApp (TAtm atom) (map TVar prms)
      super1 <- case (super0, clssDecl ^. class2super) of
        (Nothing, Nothing) -> pure Nothing
        (Nothing, Just{}) -> impossible
        (Just{}, Nothing) -> impossible
        (Just NoDict, Just (_, sclss)) -> do
          let cstr = (sclss, open t_inst)
          (dict, cstrs) <- runWriter (preSolveConstraint cstr)
          -- TODO: This is reaching too far into the internals of constraint solving.
          unless (nullSplitCstrs cstrs) $ throwUnsolvable (Just (map snd ctxt1)) cstr
          pure (Just dict)
      ds1 <- for ds0 $ \mthd -> do
        (_, MkSignDecl _ t_decl0) <- findInfo info2methods (nameOf mthd)
        -- FIXME: renameType
        let t_decl1 = t_decl0 >>= const t_inst
        inferFuncDecl mthd (open t_decl1)
      pure (MkInstDecl instName clss atom prms ctxt0 super1 ds1)

inferDecl :: forall s effs. CanInfer s effs => Decl In -> Eff effs (Maybe (Decl (Aux s)))
inferDecl = \case
  DType ds -> yield (DType ds)
  DSign{} -> pure Nothing
  DFunc func0 -> do
    t_decl :: UType _ <- open <$> typeOfFunc (nameOf func0)
    func1 <- inferFuncDecl func0 t_decl
    yield (DFunc func1)
  DExtn (MkExtnDecl name NoType s) -> do
    t <- open <$> typeOfFunc name
    yield (DExtn (MkExtnDecl name t s))
  DClss c -> yield (DClss c)
  DInst inst0 -> do
    inst1 <- inferInstDecl inst0
    yield (DInst inst1)
  where
    yield = pure . Just

inferModule' :: forall s effs.
  ( MemberST s effs
  , Members [Reader ModuleInfo, Error Failure, NameSource] effs) =>
  Module In -> Eff effs (Module (Aux s))
inferModule' = module2decls $ \decls ->
  fmap catMaybes . for decls $ \decl -> inferDecl @s decl
                                        & runGamma @s
                                        & evalSupply uvarIds
                                        & runReader (getPos decl)

freezeBind :: Bind (Aux s) -> ST s (Bind Out)
freezeBind (MkBind (x, t0) e0) = MkBind <$> ((x,) <$> freezeType t0) <*> freezeExpr e0

freezeExpr :: Expr (Aux s) -> ST s (Expr Out)
freezeExpr = \case
  ELoc le -> ELoc <$> lctd freezeExpr le
  EVar x -> pure (EVar x)
  EAtm a -> pure (EAtm a)
  ETmApp fun arg -> ETmApp <$> freezeExpr fun <*> freezeExpr arg
  ETyApp e0 t    -> ETyApp <$> freezeExpr e0 <*> freezeType t
  ECxApp e d -> ECxApp <$> freezeExpr e <*> freezeDict d
  EApp{} -> impossible
  ETmAbs binder body -> ETmAbs <$> _2 freezeType binder <*> freezeExpr body
  ETyAbs tvar body -> ETyAbs tvar <$> freezeExpr body
  ECxAbs binder body -> ECxAbs <$> (_2 . _2) freezeType binder <*> freezeExpr body
  EAbs{} -> impossible
  ELet m ds e0 -> ELet m <$> traverse freezeBind ds <*> freezeExpr e0
  EMat t e0 as -> EMat <$> freezeType t <*> freezeExpr e0 <*> traverse freezeAltn as
  ECast (c, t) e0 -> ECast . (c,) <$> freezeType t <*> freezeExpr e0
  ETyAnn t  e  -> ETyAnn <$> freezeType t <*> freezeExpr e

freezeAltn :: Altn (Aux s) -> ST s (Altn Out)
freezeAltn (MkAltn p e) = MkAltn <$> patn2type freezeType p <*> freezeExpr e

freezeFuncDecl :: FuncDecl (Aux s) tv -> ST s (FuncDecl Out tv)
freezeFuncDecl (MkFuncDecl name type0 body0) = do
  MkBind (_, type1) body1 <- freezeBind (MkBind (name, type0) body0)
  pure (MkFuncDecl name type1 body1)

freezeDecl :: Decl (Aux s) -> ST s (Decl Out)
freezeDecl = \case
  DType ds -> pure (DType ds)
  DFunc func -> DFunc <$> freezeFuncDecl func
  DExtn (MkExtnDecl func typ sym) ->
    DExtn <$> (MkExtnDecl func <$> freezeType typ <*> pure sym)
  DClss clss -> pure (DClss clss)
  DInst (MkInstDecl inst clss head prms ctxt super mthds) ->
    DInst
    <$> (MkInstDecl inst clss head prms ctxt
         <$> traverse freezeDict super
         <*> traverse freezeFuncDecl mthds)

freezeModule :: Module (Aux s) -> ST s (Module Out)
freezeModule = module2decls . traverse $ freezeDecl

inferModule :: Members [NameSource, Error Failure] effs =>
  Module In -> Eff effs (Module Out)
inferModule m0 = eitherM throwFailure pure $ runSTBelowNameSource $ runError $
  runInfo m0 (inferModule' m0) >>= sendM . freezeModule
