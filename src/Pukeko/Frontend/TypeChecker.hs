module Pukeko.FrontEnd.TypeChecker
  ( checkModule
  ) where

import Pukeko.Prelude

import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.Vector.Sized    as Vec

import           Pukeko.Pretty
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Type

checkModule :: (St.Typed st) => Module st -> Either Failure (Module st)
checkModule m0@(MkModule decls) = runTC m0 $ do
  for_ decls (lctd_ checkDecl)
  pure m0

type IsEVar ev = (HasEnv ev)

type IsTVar tv = (Eq tv, HasEnv tv, BaseTVar tv)

type TC tv ev = EffGamma tv ev [Reader ModuleInfo, Reader SourcePos, Error Failure]

runTC :: (IsType (St.StageType st)) => Module st -> TC Void Void a -> Either Failure a
runTC m0 = run . runError . runReader noPos . runInfo m0 . runGamma

checkCoercion :: Coercion (Type tv) -> TC tv ev ()
checkCoercion _ = -- (MkCoercion dir tcon t_from t_to) =
  -- FIXME: Implement the actual check.
  pure ()

typeOf :: (St.Typed st, IsEVar ev, IsTVar tv) => Expr st tv ev -> TC tv ev (Type tv)
typeOf = \case
  ELoc l -> lctd_ typeOf l
  EVar x -> lookupEVar x
  EVal z -> typeOfFunc z
  ECon c -> typeOfDCon c
  ENum _ -> pure typeInt
  EApp e0 es -> do
    t0 <- typeOf e0
    foldlM app t0 es
    where
      app tf ek = case tf of
        TFun tx ty -> check ek tx *> pure ty
        TUni{}     -> throwHere "expected type argument, but found value argument"
        _          -> throwHere "unexpected value argument"
  ELam bs e0 t0 -> do
    let ts = fmap _bind2type bs
    withinEScope ts (check e0 t0)
    pure (ts *~> t0)
  ELet ds e0 -> do
    (traverse_ . lctd_) checkDefn ds
    withBinds (fmap (_defn2bind . unlctd) ds) (typeOf e0)
  ERec ds e0 -> do
    withBinds (fmap (_defn2bind . unlctd) ds) $ do
      (traverse_ . lctd_) checkDefn ds
      typeOf e0
  EMat e0 as -> typeOfBranching typeOfAltn e0 as
  ECas e0 cs -> typeOfBranching typeOfCase e0 cs
  ECoe c e0 -> do
    checkCoercion c
    check e0 (_coeFrom c)
    pure (_coeTo c)
  ETyAbs qvs e0 -> withQVars qvs (TUni qvs <$> typeOf e0)
  ETyApp e0 ts1 -> do
    t0 <- typeOf e0
    case t0 of
      TUni qvs t1 ->
        case Vec.matchNonEmpty qvs ts1 of
          Nothing -> throwHere
            ("expected" <+> pretty (length qvs) <+> "type arguments, but found"
             <+> pretty (length ts1) <+> "type arguments")
          Just ts2 -> do
            Vec.zipWithM_ satisfiesCstrs ts2 qvs
            pure (t1 >>= scope TVar (ts2 Vec.!))
      TFun{} ->
        throwHere "expected value argument, but found type argument"
      _ -> throwHere "unexpected type argument"

satisfiesCstrs :: (IsTVar tv) => Type tv -> QVar -> TC tv ev ()
satisfiesCstrs t (MkQVar q _) = traverse_ (satisfiesCstr t) q

satisfiesCstr :: (IsTVar tv) => Type tv -> Id.Clss -> TC tv ev ()
satisfiesCstr t0 clss = do
  let (t1, tps) = gatherTApp t0
  let throwNoInst = throwHere ("no instance for" <+> pretty clss <+> parens (pretty t1))
  case t1 of
    TCon tcon -> do
      inst_mb <- lookupInfo info2insts (clss, tcon)
      case inst_mb of
        Nothing -> throwNoInst
        Just (SomeInstDecl MkInstDecl{_inst2qvars = qvsV}) -> do
          let qvs = toList qvsV
          unless (length tps == length qvs) $
            -- NOTE: This should be caught by the kind checker.
            bugWith "mitmatching number of type arguments for instance" (clss, tcon)
          zipWithM_ satisfiesCstrs tps qvs
    TVar v
      | null tps -> do
          qual <- lookupQual v
          unless (clss `Set.member` qual) throwNoInst
    _ -> throwNoInst


typeOfBranching ::
  (St.Typed st, IsTVar tv, IsEVar ev) =>
  (Type tv -> branch -> TC tv ev (Type tv)) ->
  Expr st tv ev -> NonEmpty branch -> TC tv ev (Type tv)
typeOfBranching typeOfBranch e0 (b1 :| bs) = do
  t0 <- typeOf e0
  t1 <- typeOfBranch t0 b1
  for_ bs $ \b2 -> do
    t2 <- typeOfBranch t0 b2
    unless (t1 == t2) $
      throwHere ("expected type" <+> pretty t1 <> ", but found type" <+> pretty t2)
  pure t1

typeOfAltn ::
  (St.Typed st, IsTVar tv, IsEVar ev) =>
  Type tv -> Altn st tv ev -> TC tv ev (Type tv)
typeOfAltn t (MkAltn p e) = do
  env <- patnEnvLevel p t
  withinEScope env (typeOf e)

typeOfCase ::
  (St.Typed st, IsTVar tv, IsEVar ev) =>
  Type tv -> Case st tv ev -> TC tv ev (Type tv)
typeOfCase t (MkCase c ts bs e0) = do
  let ps = map PVar (toList bs)
      e1 = fmap (first (bs Vec.!)) e0
  typeOfAltn t (MkAltn (PCon c ts ps) e1)

patnEnvLevel ::
  (IsTVar tv) => Patn Type tv -> Type tv -> TC tv ev (EnvLevelOf Id.EVar (Type tv))
patnEnvLevel p t0 = case p of
  PWld -> pure Map.empty
  PVar x -> pure (Map.singleton x t0)
  PCon c ts1 ps -> do
    Some1 (Pair1 _tconDecl (MkDConDecl tcon dcon _tag flds1)) <- findInfo info2dcons c
    let t1 = mkTApp (TCon tcon) (toList ts1)
    unless (t0 == t1) $ throwHere
      ("expected pattern of type" <+> pretty t0
       <> ", but found pattern of type" <+> pretty t1)
    unless (length flds1 == length ps) $ throwHere
      ("expected" <+> pretty (length flds1) <+> "pattern arguments of" <+> pretty dcon
       <> ", but found" <+> pretty (length ps) <+> "pattern arguments")
    Vec.withList ts1 $ \ts2 -> do
      let fldsProxy :: [Type (TFinScope n tv)] -> Proxy n
          fldsProxy _ = Proxy
      case sameNat (Vec.plength ts2) (fldsProxy flds1) of
        Nothing -> bugWith "mismatching kinds for type constructor" tcon
        Just Refl -> do
          let t_ps = map (>>= scope absurd (ts2 Vec.!)) flds1
          Map.unions <$> zipWithM patnEnvLevel ps t_ps

match :: (IsTVar tv) => Type tv -> Type tv -> TC tv ev ()
match t0 t1 =
  unless (t0 == t1) $
    throwHere ("expected type" <+> pretty t0 <> ", but found type" <+> pretty t1)

check :: (St.Typed st, IsTVar tv, IsEVar ev) => Expr st tv ev -> Type tv -> TC tv ev ()
check e t0 = typeOf e >>= match t0

checkDefn :: (St.Typed st, IsEVar ev, IsTVar tv) => Defn st tv ev -> TC tv ev ()
checkDefn (MkDefn (MkBind _ t) e) = check e t

checkDecl :: (St.Typed st) => Decl st -> TC Void Void ()
checkDecl = \case
  DType{} -> pure ()
  DSign{} -> pure ()
  DClss{} -> pure ()
  DInst (MkInstDecl _ tcon qvs ds) -> do
    let t_inst = mkTApp (TCon tcon) (imap (\i -> TVar . mkBound i . _qvar2tvar) qvs)
    -- FIXME: Ensure that the type in @b@ is correct as well.
    withQVars qvs $ for_ ds $ lctd_ $ \(MkDefn b e) -> do
      (_, MkSignDecl _ t_mthd) <- findInfo info2mthds (b^.bind2evar)
      let t_decl = renameType (t_mthd >>= scope absurd (const t_inst))
      check e t_decl
  DDefn d -> checkDefn d
  DSupC (MkSupCDecl z qvs t0 bs e0) -> do
      t1 <- withQVars qvs (withBinds bs (typeOf e0))
      let t2 = fmap _bind2type bs *~> t1
      match (mkTUni qvs t0) (mkTUni qvs t2)
    `catchError` \e -> throwFailure ("while type checking" <+> pretty z <+> ":" $$ e)
  DPrim _ -> pure ()
