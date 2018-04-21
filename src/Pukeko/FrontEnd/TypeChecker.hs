module Pukeko.FrontEnd.TypeChecker
  ( check
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import qualified Bound.Name as B
import qualified Data.Map.Extended as Map

import           Pukeko.AST.Dict
import           Pukeko.AST.Expr
import           Pukeko.AST.Expr.Optics (patn2binder)
import           Pukeko.AST.Language
import qualified Pukeko.AST.SuperCore as Core
import qualified Pukeko.AST.SystemF as SysF
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info

type CanTC effs =
  (CanGamma effs, Members [Reader ModuleInfo, Reader SourcePos, Error Failure] effs)
type TC a = forall effs. CanTC effs => Eff effs a

class HasModuleInfo m => TypeCheckable m where
  checkModule :: m -> TC ()

check :: (Member (Error Failure) effs, TypeCheckable m) => m -> Eff effs ()
check m0 = either throwFailure pure .
  run . runError . runReader noPos . runInfo m0 . runGamma $ checkModule m0

checkCoercion :: Coercion -> Type -> Type -> TC ()
checkCoercion (MkCoercion _dir _tcon) _from _to =
  -- FIXME: Implement the actual check.
  pure ()

typeOf :: IsTyped lg => Expr lg -> TC Type
typeOf = \case
  ELoc le -> here le $ typeOf (le^.lctd)
  EVar x -> lookupTmVar x
  EAtm a -> typeOfAtom a
  EApp e0 a -> do
    t0 <- typeOf e0
    case (a, t0) of
      (TmArg e1, TFun tx ty) -> checkExpr e1 tx $> ty
      (TmArg{}, TUni{}) -> throwHere "expected type argument, but found value argument"
      (TmArg{}, _)      -> throwHere "unexpected value argument"
      (TyArg t1, TUni _ tq) -> pure (B.instantiate1Name t1 tq)
      (TyArg{}, TFun{}) -> throwHere "expected value argument, but found type argument"
      (TyArg{}, _     ) -> throwHere "unexpected type argument"
      (DxArg dict, TCtx cstr t1) -> do
        checkDict dict cstr
        pure t1
      (DxArg{}, _) -> throwHere "unexpected dict argument"
  EAbs par e0 ->
    case par of
      TmPar binder -> TFun (snd binder) <$> introTmVar binder (typeOf e0)
      TyPar v      -> TUni' v           <$> introTyVar v      (typeOf e0)
      DxPar binder -> TCtx (snd binder) <$> introDxVar binder (typeOf e0)
  ELet BindPar ds e0 -> do
    traverse_ checkBind ds
    introTmVars (map _b2binder ds) (typeOf e0)
  ELet BindRec ds e0 ->
    introTmVars (map _b2binder ds) $ do
      traverse_ checkBind ds
      typeOf e0
  EMat t0 e0 (a1 :| as) -> do
    checkExpr e0 t0
    t1 <- typeOfAltn a1
    for_ as $ \a2 -> do
      t2 <- typeOfAltn a2
      unless (t1 == t2) $
        throwHere ("expected type" <+> pretty t1 <> ", but found type" <+> pretty t2)
    pure t1
  ECast (coe, t_to) e0 -> do
    t_from <- typeOf e0
    checkCoercion coe t_from t_to
    pure t_to
  ETyAnn t0 e0 -> checkExpr e0 t0 $> t0

checkDict :: Dict -> TypeCstr -> TC ()
checkDict dict cstr =
  case dict of
    DVar x -> lookupDxVar x >>= matchCstr cstr
    DDer z targs subDicts ->
      -- TODO: Use 'findInfo'
      lookupInfo info2dicts z >>= \case
        Nothing -> impossible
        Just (SomeInstDecl (SysF.MkInstDecl _ clss tatom prms ctxt _ _)) -> do
          matchCstr cstr (clss, foldl TApp (TAtm tatom) targs)
          -- the kind checker guarantees matching parameter/argument arity
          let inst = (>>= (Map.fromList (zipExact prms targs) Map.!))
          for_ (zipExact subDicts ctxt) $ \(subDict, (_, subCstr)) ->
            checkDict subDict (second inst subCstr)
    DSub z clss typ subDict -> do
      clssDecl <- findInfo info2classes clss
      case clssDecl ^. SysF.class2super of
        Nothing -> impossible
        Just (z', super)
          | z /= z' -> impossible
          | otherwise -> do
              matchCstr cstr (super, typ)
              checkDict subDict (clss, typ)
  where
    matchCstr cstr0 cstr1 =
      unless (cstr0 == cstr1) $
        throwHere ("expected evidence for" <+> prettyCstr cstr0 <> ","
                   <+> "but found evidence for" <+> prettyCstr cstr1)

typeOfAltn :: IsTyped lg => Altn lg -> TC Type
typeOfAltn (MkAltn p e) =
  introTmVars (toListOf patn2binder p) (typeOf e)

match :: Type -> Type -> TC ()
match t0 t1 =
  unless (t0 == t1) $
    throwHere ("expected type" <+> pretty t0 <> ", but found type" <+> pretty t1)

checkExpr :: IsTyped lg => Expr lg -> Type -> TC ()
checkExpr e t0 = typeOf e >>= match t0

checkBind :: IsTyped lg => Bind lg -> TC ()
checkBind (MkBind (_, t) e) = checkExpr e t

instance (IsTyped lg, IsTyped lg) => TypeCheckable (SysF.Module lg) where
  checkModule (SysF.MkModule decls) = for_ decls $ here' $ \case
    SysF.DType{} -> pure ()
    SysF.DSign{} -> pure ()
    SysF.DFunc (SysF.MkFuncDecl _ typ_ body) -> checkExpr body typ_
    SysF.DExtn _ -> pure ()
    SysF.DClss{} -> pure ()
    SysF.DInst (SysF.MkInstDecl inst clss atom prms ctxt super ds) -> do
      clssDecl <- findInfo info2classes clss
      let t_inst = foldl TApp (TAtm atom) (map TVar prms)
      -- FIXME: Ensure that the type in @b@ is correct as well.
      introTyVars prms $ introDxVars ctxt $ do
        case (super, clssDecl ^. SysF.class2super) of
          (Nothing, Nothing) -> pure ()
          (Nothing, Just _) -> throwHere $
            "instance" <+> pretty inst <+> "wronlgy indicates that class"
            <+> pretty clss <+> "has a super class"
          (Just _, Nothing) -> throwHere $
            "instance" <+> pretty inst <+> "wrongly indicated that class"
            <+> pretty clss <+> "has no super class"
          (Just dict, Just (_, sclss)) ->
            checkDict dict (sclss, t_inst)
        for_ ds $ \(SysF.MkFuncDecl name _typ body) -> do
          (_, SysF.MkSignDecl _ t_mthd) <- findInfo info2methods name
          -- TODO: There might be some lexical name capturing going on here: type
          -- variables with different IDs could still share the same lexical name.
          -- Since this is a UX thing and all type errors here are compiler bugs,
          -- we don't put any effort into correcting this until we finally hit
          -- that problem.
          let t_decl = t_mthd >>= const t_inst
          checkExpr body t_decl

instance TypeCheckable Core.Module where
  checkModule (Core.MkModule _types _extns supcs) =
    for_ supcs $ \(Core.SupCDecl z t_decl pars e0) -> do
        t0 <- introPars pars $ typeOf e0
        match (vacuous t_decl) (rewindr mkTAbs pars t0)
      `catchError` \e -> throwFailure ("while type checking" <+> pretty z <+> ":" $$ e)
