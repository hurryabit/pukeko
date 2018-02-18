module Pukeko.FrontEnd.TypeChecker
  ( check
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import qualified Data.List.NE as NE
import qualified Data.Map     as Map
import qualified Data.Set     as Set

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.Expr
import           Pukeko.AST.Name
import qualified Pukeko.AST.SystemF   as SysF
import qualified Pukeko.AST.SuperCore as Core
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Type

type IsEVar ev = (HasEnv ev)

type IsTVar tv = (Eq tv, HasEnv tv, BaseTVar tv)

type TC tv ev = EffGamma tv ev [Reader ModuleInfo, Reader SourcePos, Error Failure]

class HasModuleInfo m => TypeCheckable m where
  checkModule :: m -> TC Void Void ()

check :: (Member (Error Failure) effs, TypeCheckable m) => m -> Eff effs ()
check m0 = either throwError pure .
  run . runError . runReader noPos . runInfo m0 . runGamma $ checkModule m0

checkCoercion :: Coercion -> Type tv -> Type tv -> TC tv ev ()
checkCoercion (MkCoercion _dir _tcon) _from _to =
  -- FIXME: Implement the actual check.
  pure ()

typeOf :: (IsTyped st, IsEVar ev, IsTVar tv) => Expr st tv ev -> TC tv ev (Type tv)
typeOf = \case
  ELoc le -> here le $ typeOf (le^.lctd)
  EVar x -> lookupEVar x
  EAtm a -> typeOfAtom a
  EApp fun arg -> do
    t_fun <- typeOf fun
    case t_fun of
      TFun tx ty -> checkExpr arg tx $> ty
      TUni{}     -> throwHere "expected type argument, but found value argument"
      _          -> throwHere "unexpected value argument"
  ELam (MkBind _ t_param) body -> do
    t_body <- withinEScope1 id t_param (typeOf body)
    pure (t_param ~> t_body)
  ELet ds e0 -> do
    traverse_ checkDefn ds
    withinEScope' (_bind2type . _defn2bind) ds (typeOf e0)
  ERec ds e0 -> do
    withinEScope' (_bind2type . _defn2bind) ds $ do
      traverse_ checkDefn ds
      typeOf e0
  EMat e0 (a1 :| as) -> do
    t0 <- typeOf e0
    t1 <- typeOfAltn t0 a1
    for_ as $ \a2 -> do
      t2 <- typeOfAltn t0 a2
      unless (t1 == t2) $
        throwHere ("expected type" <+> pretty t1 <> ", but found type" <+> pretty t2)
    pure t1
  ETyAnn t_to (ETyCoe c e0) -> do
    t_from <- typeOf e0
    checkCoercion c t_from t_to
    pure t_to
  ETyCoe{} ->
    bug "type coercion without surrounding type annotation durch type checking"
  ETyAbs qvs e0 -> withQVars qvs (TUni qvs <$> typeOf e0)
  ETyApp e0 ts1 -> do
    t0 <- typeOf e0
    case t0 of
      TUni qvs tq -> do
        unless (length qvs == length ts1) $
          throwHere ("expected" <+> pretty (length qvs) <+> "type arguments, but found"
                     <+> pretty (length ts1) <+> "type arguments")
        NE.zipWithM_ satisfiesCstrs ts1 qvs
        pure (instantiateN ts1 tq)
      TFun{} ->
        throwHere "expected value argument, but found type argument"
      _ -> throwHere "unexpected type argument"
  ETyAnn t0 e0 -> checkExpr e0 t0 *> pure t0

satisfiesCstrs :: (IsTVar tv) => Type tv -> QVar -> TC tv ev ()
satisfiesCstrs t (MkQVar q _) = traverse_ (satisfiesCstr t) q

satisfiesCstr :: (IsTVar tv) => Type tv -> Name Clss -> TC tv ev ()
satisfiesCstr t0 clss = do
  let (t1, tps) = gatherTApp t0
  let throwNoInst = throwHere ("TC: no instance for" <+> pretty clss <+> parens (pretty t1))
  case t1 of
    TAtm atom -> do
      inst_mb <- lookupInfo info2insts (clss, atom)
      case inst_mb of
        Nothing -> throwNoInst
        Just (SomeInstDecl SysF.MkInstDecl{_inst2qvars = qvsV}) -> do
          let qvs = toList qvsV
          unless (length tps == length qvs) $
            -- NOTE: This should be caught by the kind checker.
            bugWith "mitmatching number of type arguments for instance" (clss, atom)
          zipWithM_ satisfiesCstrs tps qvs
    TVar v
      | null tps -> do
          qual <- lookupQual v
          unless (clss `Set.member` qual) throwNoInst
    _ -> throwNoInst

typeOfAltn ::
  (IsTyped st, IsTVar tv, IsEVar ev) =>
  Type tv -> Altn st tv ev -> TC tv ev (Type tv)
typeOfAltn t (MkAltn p e) = do
  env <- patnEnvLevel p t
  withinEScope id env (typeOf e)

patnEnvLevel :: forall lg tv ev. (TypeOf lg ~ Type, IsTVar tv) =>
  Patn lg tv -> Type tv -> TC tv ev (EnvLevelOf Id.EVar (Type tv))
patnEnvLevel p t0 = case p of
  PWld -> pure Map.empty
  PVar x -> pure (Map.singleton x t0)
  PCon c ts1 ps -> do
    (_tconDecl, MkDConDecl tcon dcon _tag flds1) <- findInfo info2dcons c
    let t1 = mkTApp (TCon tcon) (toList ts1)
    unless (t0 == t1) $ throwHere
      ("expected pattern of type" <+> pretty t0
       <> ", but found pattern of type" <+> pretty t1)
    unless (length flds1 == length ps) $ throwHere
      ("expected" <+> pretty (length flds1) <+> "pattern arguments of" <+> pretty dcon
       <> ", but found" <+> pretty (length ps) <+> "pattern arguments")
    -- NOTE: If the instantiation fails, the field type contains type
    -- variables not mentioned in the parameter list of the type constructor.
    -- The renamer should have caught this.
    let t_ps = map (instantiateN' ts1) flds1
    Map.unions <$> zipWithM patnEnvLevel ps t_ps
  PSimple c ts1 bs -> patnEnvLevel @Typed (PCon c ts1 (map (maybe PWld PVar) bs)) t0

match :: (IsTVar tv) => Type tv -> Type tv -> TC tv ev ()
match t0 t1 =
  unless (t0 == t1) $
    throwHere ("expected type" <+> pretty t0 <> ", but found type" <+> pretty t1)

checkExpr :: (IsTyped st, IsTVar tv, IsEVar ev) =>
  Expr st tv ev -> Type tv -> TC tv ev ()
checkExpr e t0 = typeOf e >>= match t0

checkDefn :: (IsTyped st, IsEVar ev, IsTVar tv) => Defn st tv ev -> TC tv ev ()
checkDefn (MkDefn (MkBind _ t) e) = checkExpr e t

instance IsTyped st => TypeCheckable (SysF.Module st) where
  checkModule (SysF.MkModule decls) = for_ decls $ \case
    SysF.DType{} -> pure ()
    SysF.DSign{} -> pure ()
    SysF.DFunc (SysF.MkFuncDecl _ typ_ body) -> checkExpr body typ_
    SysF.DExtn _ -> pure ()
    SysF.DClss{} -> pure ()
    SysF.DInst (SysF.MkInstDecl _ atom qvs ds) -> do
      let t_inst = mkTApp (TAtm atom) (imap (\i -> TVar . mkBound i . _qvar2tvar) qvs)
      -- FIXME: Ensure that the type in @b@ is correct as well.
      withQVars qvs $ for_ ds $ \(SysF.MkFuncDecl (unlctd -> name) _typ body) -> do
        (_, SysF.MkSignDecl _ t_mthd) <- findInfo info2mthds name
        let t_decl = renameType (instantiate' (const t_inst) t_mthd)
        checkExpr body t_decl

instance TypeCheckable Core.Module where
  checkModule (Core.MkModule _types _extns supcs) =
    for_ supcs $ \(Core.SupCDecl z t_decl qvs bs e0) -> do
        t0 <- withQVars qvs (withinEScope' _bind2type bs (typeOf e0))
        match t_decl (mkTUni qvs (fmap _bind2type bs *~> t0))
      `catchError` \e -> throwFailure ("while type checking" <+> pretty z <+> ":" $$ e)
