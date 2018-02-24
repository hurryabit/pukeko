module Pukeko.FrontEnd.TypeChecker
  ( check
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import qualified Bound.Name as B
import qualified Data.List.NE as NE
import qualified Data.Map.Extended as Map
import qualified Data.Set     as Set

import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.Expr
import           Pukeko.AST.Name
import qualified Pukeko.AST.SystemF   as SysF
import qualified Pukeko.AST.SuperCore as Core
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Type

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
  EVar x -> lookupEVar x
  EAtm a -> typeOfAtom a
  EApp fun arg -> do
    t_fun <- typeOf fun
    case t_fun of
      TFun tx ty -> checkExpr arg tx $> ty
      TUni{}     -> throwHere "expected type argument, but found value argument"
      _          -> throwHere "unexpected value argument"
  ELam binder body -> do
    t_body <- withinEScope1 binder (typeOf body)
    pure (snd binder ~> t_body)
  ELet ds e0 -> do
    traverse_ checkDefn ds
    withinEScope (map _defn2bind ds) (typeOf e0)
  ERec ds e0 -> do
    withinEScope (map _defn2bind ds) $ do
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
  ETyCoe{} -> impossible  -- the type inferencer puts type annotations around coercions
  ETyAbs qvs e0 ->
    withinTScope qvs (TUni qvs . B.abstractName (env Map.!?) <$> typeOf e0)
    where env = Map.fromList (zipWithFrom (\i (v, _) -> (v, i)) 0 (toList qvs))
  ETyApp e0 ts1 -> do
    t0 <- typeOf e0
    case t0 of
      TUni qvs tq -> do
        unless (length qvs == length ts1) $
          throwHere ("expected" <+> pretty (length qvs) <+> "type arguments, but found"
                     <+> pretty (length ts1) <+> "type arguments")
        NE.zipWithM_ satisfiesCstrs ts1 qvs
        pure (B.instantiateName (toList ts1 !!) tq)
      TFun{} ->
        throwHere "expected value argument, but found type argument"
      _ -> throwHere "unexpected type argument"
  ETyAnn t0 e0 -> checkExpr e0 t0 *> pure t0

satisfiesCstrs :: Type -> TVarBinder -> TC ()
satisfiesCstrs t = traverse_ (satisfiesCstr t) . snd

satisfiesCstr :: Type -> Name Clss -> TC ()
satisfiesCstr (gatherTApp -> (t1, targs)) clss = do
  let throwNoInst =
        throwHere ("TC: no instance for" <+> pretty clss <+> parens (pretty t1))
  case t1 of
    TAtm atom ->
      lookupInfo info2insts (clss, atom) >>= \case
        Nothing -> throwNoInst
        Just (SomeInstDecl inst) ->
          -- the kind checker guarantees parameter/argument arity
          sequence_ (zipWithExact satisfiesCstrs targs (SysF._inst2params inst))
    TVar v
      | null targs -> do
          qual <- lookupTVar v
          unless (clss `Set.member` qual) throwNoInst
    _ -> throwNoInst

typeOfAltn :: IsTyped lg => Type -> Altn lg -> TC Type
typeOfAltn t (MkAltn p e) = do
  env <- patnEnvLevel p t
  withinEScope (Map.toList env) (typeOf e)

patnEnvLevel :: TypeOf lg ~ Type => Patn lg -> Type -> TC (Map NameEVar Type)
patnEnvLevel p t0 = case p of
  PWld -> pure Map.empty
  PVar x -> pure (Map.singleton x t0)
  PCon c ts1 ps -> do
    (MkTConDecl _ params _, MkDConDecl tcon dcon _tag flds1) <- findInfo info2dcons c
    let t1 = mkTApp (TCon tcon) (toList ts1)
    unless (t0 == t1) $ throwHere
      ("expected pattern of type" <+> pretty t0
       <> ", but found pattern of type" <+> pretty t1)
    unless (length flds1 == length ps) $ throwHere
      ("expected" <+> pretty (length flds1) <+> "pattern arguments of" <+> pretty dcon
       <> ", but found" <+> pretty (length ps) <+> "pattern arguments")
    let env = Map.fromList (zipExact params ts1)
    -- NOTE: If the instantiation fails, the field type contains type
    -- variables not mentioned in the parameter list of the type constructor.
    -- The renamer should have caught this.
    let t_ps = map (>>= (env Map.!)) flds1
    Map.unions <$> zipWithM patnEnvLevel ps t_ps
  PSimple c ts1 bs -> patnEnvLevel @Typed (PCon c ts1 (map (maybe PWld PVar) bs)) t0

match :: Type -> Type -> TC ()
match t0 t1 =
  unless (t0 == t1) $
    throwHere ("expected type" <+> pretty t0 <> ", but found type" <+> pretty t1)

checkExpr :: IsTyped lg => Expr lg -> Type -> TC ()
checkExpr e t0 = typeOf e >>= match t0

checkDefn :: IsTyped lg => Defn lg -> TC ()
checkDefn (MkDefn (_, t) e) = checkExpr e t

instance IsTyped st => TypeCheckable (SysF.Module st) where
  checkModule (SysF.MkModule decls) = for_ decls $ \case
    SysF.DType{} -> pure ()
    SysF.DSign{} -> pure ()
    SysF.DFunc (SysF.MkFuncDecl _ typ_ body) -> checkExpr body typ_
    SysF.DExtn _ -> pure ()
    SysF.DClss{} -> pure ()
    SysF.DInst (SysF.MkInstDecl _ _ atom qvs ds) -> do
      let t_inst = mkTApp (TAtm atom) (map (TVar . fst) qvs)
      -- FIXME: Ensure that the type in @b@ is correct as well.
      withinTScope qvs $ for_ ds $ \(SysF.MkFuncDecl name _typ body) -> do
        (_, SysF.MkSignDecl _ t_mthd) <- findInfo info2mthds name
        -- TODO: There might be some lexical name capturing going on here: type
        -- variables with different IDs could still share the same lexical name.
        -- Since this is a UX thing and all type errors here are compiler bugs,
        -- we don't put any effort into correcting this until we finally hit
        -- that problem.
        let t_decl = t_mthd >>= const t_inst
        checkExpr body t_decl

instance TypeCheckable Core.Module where
  checkModule (Core.MkModule _types _extns supcs) =
    for_ supcs $ \(Core.SupCDecl z t_decl qvs bs e0) -> do
        t0 <- withinTScope qvs $ withinEScope bs $ typeOf e0
        match (vacuous t_decl) (mkTUni qvs (fmap snd bs *~> t0))
      `catchError` \e -> throwFailure ("while type checking" <+> pretty z <+> ":" $$ e)
