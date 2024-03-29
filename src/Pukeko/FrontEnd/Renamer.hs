{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.FrontEnd.Renamer
  ( renameModule
  ) where

import Pukeko.Prelude

import           Control.Lens (matching)
import           Control.Monad.Extra
import           Data.Bitraversable
import           Data.List.Extra (nubOrd)
import qualified Data.Map.Extended as Map

import           Pukeko.AST.ConDecl
import           Pukeko.AST.Dict
import           Pukeko.AST.Language
import           Pukeko.AST.Name
import qualified Pukeko.AST.Operator as Op
import qualified Pukeko.AST.Surface as Ps
import           Pukeko.AST.SystemF
import           Pukeko.AST.Type

type Out = Surface

data Tabs = Tabs
  { _binopTab :: Map Op.Binary TmVar
  , _tconTab  :: Map (Ps.Name 'TyCon) (GenDecl (Only 'TyCon) Out)
  , _dconTab  :: Map (Ps.Name 'TmCon) TmConDecl
  , _evarTab  :: Map (Ps.Name 'TmVar) (GenDecl (Only 'TmVar) Out)
  }

type Env = Map (Ps.Name 'TmVar) TmVar

type GlobalEffs effs = (Members [State Tabs, NameSource] effs, CanThrowHere effs)

type LocalEffs effs = (GlobalEffs effs, Member (Reader Env) effs)

makeLenses ''Tabs

-- | Create a new global 'Binder' if the name has not been declared before in
-- the same namespace and pass the result to the continuation. The continuations
-- result is finally stored in the declaration table.
introGlobal :: GlobalEffs effs =>
  Lens' Tabs (Map (Ps.Name nsp) decl) ->
  Failure ->
  Ps.LctdName nsp ->
  (a -> decl) ->
  (Name nsp -> Eff effs a) ->
  Eff effs a
introGlobal tab desc lname@(Lctd pos name) mkDecl cont = do
  whenM (uses tab (Map.member name)) $
    throwAt pos ("duplicate" <+> desc <:~> pretty name)
  binder <- mkName lname
  res <- cont binder
  modifying tab (Map.insert name (mkDecl res))
  pure res

lookupGlobal :: (GlobalEffs effs) =>
  Lens' Tabs (Map (Ps.Name nsp) gendecl) ->
  Prism' gendecl decl ->
  Failure ->
  Ps.LctdName nsp ->
  Eff effs decl
lookupGlobal tab sel desc (Lctd pos name) =
  uses tab (Map.lookup name) >>= \case
    Nothing -> throwAt pos (desc <:~> pretty name)
    Just gendecl ->
      case matching sel gendecl of
        Left _     -> throwAt pos ("not a" <+> desc <:~> pretty name)
        Right decl -> pure decl

lookupGlobalName :: (GlobalEffs effs, NameSpaceOf decl ~ nsp, HasName decl) =>
  Lens' Tabs (Map (Ps.Name nsp) gendecl) ->
  Prism' gendecl decl ->
  Failure ->
  Ps.LctdName nsp ->
  Eff effs (Name nsp)
lookupGlobalName tab sel desc name = nameOf <$> lookupGlobal tab sel desc name

lookupBinop :: GlobalEffs effs => Op.Binary -> Eff effs TmVar
lookupBinop op = do
  fun_mb <- uses binopTab (Map.lookup op)
  case fun_mb of
    Just fun -> pure fun
    Nothing  -> throwHere ("unknown operator:" <+> pretty op)

introLocal
  :: LocalEffs effs
  => Ps.LctdName 'TmVar
  -> (TmBinder NoType -> Eff effs a)
  -> Eff effs a
introLocal name cont = do
  binder <- mkName name
  local (Map.insert (unlctd name) binder) $ cont (binder, NoType)

introLocals
  :: LocalEffs effs
  => [Ps.LctdName 'TmVar]
  -> ([TmBinder NoType] -> Eff effs a)
  -> Eff effs a
introLocals names0 cont = go names0 []
  where
    go [] binders = cont (reverse binders)
    go (name:names) binders = introLocal name $ \binder -> go names (binder:binders)

lookupLocal :: LocalEffs effs => Ps.LctdName 'TmVar -> Eff effs (Maybe TmVar)
lookupLocal name = asks (Map.lookup (unlctd name))


-- * Renaming of types

-- | Rename an atomic type. Does basically nothing unless it is a reference to a
-- type constructor.
rnTypeAtom :: GlobalEffs effs => Ps.TypeAtom -> Eff effs TypeAtom
rnTypeAtom = \case
  Ps.TAArr -> pure TAArr
  Ps.TAInt -> pure TAInt
  Ps.TACon name ->
    TACon <$> lookupGlobalName tconTab _DType "unknown type constructor" name

lookupTyVar
  :: GlobalEffs effs
  => Ps.LctdName 'TyVar -> Map (Ps.Name 'TyVar) TyVar -> Eff effs TyVar
lookupTyVar (unlctd -> x) env =
  case Map.lookup x env of
    Just v  -> pure v
    Nothing -> throwHere ("unknown type variable:" <+> pretty x)

-- | Rename a type under the assumption that all its free variables are
-- contained in the map. All references to type constructors are renamed as
-- well.
rnType :: GlobalEffs effs => Map (Ps.Name 'TyVar) TyVar -> Ps.Type -> Eff effs Type
rnType env = go
  where
    go = \case
      Ps.TVar x -> TVar <$> lookupTyVar x env
      Ps.TAtm a -> TAtm <$> rnTypeAtom a
      Ps.TApp tf tp -> TApp <$> go tf <*> go tp

-- | Collect all type class/type variable pairs in a type constraint. Rename all
-- references to type classes.
rnConstraints :: GlobalEffs effs =>
  Map (Ps.Name 'TyVar) TyVar -> Ps.TypeCstr -> Eff effs [(Class, TyVar)]
rnConstraints env (Ps.MkTypeCstr constraints) =
  fmap nubOrd .
  for constraints $ \(clss, tvar0) -> do
    -- TODO: Applicative.
    tvar1 <- lookupTyVar tvar0 env
    (, tvar1) <$> lookupGlobalName tconTab _DClss "unknown type class" clss

-- | Rename a type scheme. Fails if there are constraints on type variables
-- which are contained in the map or not in the type.
rnTypeScheme :: GlobalEffs effs =>
  Map (Ps.Name 'TyVar) TyVar -> Ps.TypeScheme -> Eff effs Type
rnTypeScheme env0 (Ps.MkTypeScheme cstrs0 t0) = do
  env1 <- traverse mkName (Ps.freeTyVars t0 `Map.difference` env0)
  cstrs1 <- rnConstraints env1 cstrs0
  t1 <- rnType (env1 <> env0) t0
  pure (rewindr TUni' (Map.elems env1) (rewindr TCtx (map (second TVar) cstrs1) t1))

-- | Rename a type coercion.
rnCoercion :: GlobalEffs effs => Ps.Coercion -> Eff effs Coercion
rnCoercion (Ps.MkCoercion dir0 tname) = do
  let dir1 = case dir0 of
        Ps.Inject  -> Inject
        Ps.Project -> Project
  MkCoercion dir1 <$> lookupGlobalName tconTab _DType "unknown type constructor" tname


-- * Renaming of expressions

-- | Rename a pattern.
rnPatn :: LocalEffs effs => Ps.Patn -> (Patn Out -> Eff effs a) -> Eff effs a
rnPatn patn cont = case patn of
  Ps.PWld -> cont PWld
  Ps.PVar name ->
    introLocal name $ cont . PVar
  Ps.PCon dcon0 patns0 -> do
    dcon1 <- lookupGlobalName dconTab id "unknown data constructor" dcon0
    let go [] outPatns = cont (PCon dcon1 (reverse outPatns))
        go (inPatn:inPatns) outPatns =
          rnPatn inPatn $ \outPatn -> go inPatns (outPatn:outPatns)
    go patns0 []

-- | Rename a pattern match alternative.
rnAltn :: LocalEffs effs => Ps.Altn (Ps.LctdName 'TmVar) -> Eff effs (Altn Out )
rnAltn (Ps.MkAltn patn0 rhs) = rnPatn patn0 $ \patn1 -> MkAltn patn1 <$> rnExpr rhs

-- | Rename a value abstraction (lambda).
rnELam :: LocalEffs effs =>
  [Ps.LctdName 'TmVar] -> Ps.Expr (Ps.LctdName 'TmVar) -> Eff effs (Expr Out)
rnELam names body = introLocals names $ \binders ->
  rewindr ETmAbs binders <$> rnExpr body

-- | Rename an expression.
rnExpr :: LocalEffs effs => Ps.Expr (Ps.LctdName 'TmVar) -> Eff effs (Expr Out)
rnExpr = \case
  Ps.ELoc le -> here le $ ELoc <$> lctd rnExpr le
  Ps.EVar x ->
    lookupLocal x >>= \case
      Just y -> pure (EVar y)
      Nothing -> EVal <$> lookupGlobalName evarTab _DSign "unknown variable" x
  Ps.ECon name ->
    ECon <$> lookupGlobalName dconTab id "unknown data constructor" name
  Ps.ENum n -> pure (ENum n)
  Ps.EApp e0 es -> foldl ETmApp <$> rnExpr e0 <*> traverse rnExpr es
  Ps.EOpp op e1 e2 ->
    ETmApp <$> (ETmApp <$> (EVal <$> lookupBinop op) <*> rnExpr e1) <*> rnExpr e2
  Ps.EMat e0 as0 ->
    case as0 of
      []   -> throwHere "pattern match without alternatives"
      a:as -> EMat NoType <$> rnExpr e0 <*> traverse rnAltn (a :| as)
  Ps.ELam (toList -> bs0) e0 -> rnELam bs0 e0
  Ps.ELet (toList -> binds0) body0 -> do
    let (binders0, exprs0) =
          unzip (map (\(Ps.MkBind binder expr) -> (binder, expr)) binds0)
    exprs1 <- traverse rnExpr exprs0
    introLocals binders0 $ \names -> do
      let binds1 = zipWithExact TmNonRec names exprs1
      body1 <- rnExpr body0
      pure (rewindr ELet binds1 body1)
  Ps.ERec (toList -> binds0) body0 -> do
    let (binders0, exprs0) =
          unzip (map (\(Ps.MkBind binder expr) -> (binder, expr)) binds0)
    introLocals binders0 $ \names -> do
      exprs1 <- traverse rnExpr exprs0
      let binds1 = zipWithExact (,) names exprs1
      body1 <- rnExpr body0
      pure (ELet (TmRec binds1) body1)
  Ps.ECoe c e -> ECast . (, NoType) <$> rnCoercion c <*> rnExpr e


-- * Renaming of top level declarations

-- | Rename a data constructor declaration. Assumes that the corresponding type
-- constructor (or at least some dummy version of it) has already been
-- introduced to the global environment.
rnTmConDecl :: GlobalEffs effs =>
  TyCon -> Map (Ps.Name 'TyVar) TyVar -> Int -> Ps.TmConDecl -> Eff effs TmConDecl
rnTmConDecl tcon env tag (Ps.MkTmConDecl name fields) = here name $
  introGlobal dconTab "data constructor" name id $ \dcon ->
    MkTmConDecl tcon dcon tag <$> traverse (rnType env) fields

-- | Rename a (potentially) recursive type declaration.
rnTypeDecl :: GlobalEffs effs => Ps.TyConDecl -> Eff effs TyConDecl
rnTypeDecl (Ps.MkTyConDecl name params0 dcons0) = do
  -- NOTE: Since the type definition might be recursive, we need to introduce a
  -- dummy version of the type constructor first and overwrite it in the end.
  let mkDummy binder = DType (MkTyConDecl binder [] (Right []))
  binder <- introGlobal tconTab "type constructor" name mkDummy pure
  params1 <- traverse mkName params0
  let env = Map.fromList (zip (map unlctd params0) params1)
  dcons1 <- bitraverse (rnType env) (zipWithM (rnTmConDecl binder env) [0..]) dcons0
  let tcon = MkTyConDecl binder params1 dcons1
  modifying tconTab (Map.insert (unlctd name) (DType tcon))
  pure tcon

-- | Rename a signature declaration. For signatures of top level functions, we
-- have @tv = Void@, the map is empty and the function is identity. For method
-- signatures, we have @tv = TScope Int Void@, the map contains the class
-- paramaters and the function adds universal quantification over the class
-- parameters.
rnSignDecl :: GlobalEffs effs =>
  Map (Ps.Name 'TyVar) TyVar -> (Type -> Type) -> Ps.SignDecl -> Eff effs (SignDecl tv)
rnSignDecl env preClose (Ps.MkSignDecl binder typeScheme) =
  introGlobal evarTab "function declaration" binder mkDSign $ \name ->
    MkSignDecl name <$> rnTypeScheme env typeScheme
  where
    mkDSign = DSign . over sign2type preClose

-- | Rename a function declaration. The filled in type is bogus. For top level
-- functions, we have @tv = Void@. For method definitions, we have @tv = TScope
-- Int Void@.
rnFuncDecl :: GlobalEffs effs => Ps.Bind (Ps.LctdName 'TmVar) -> Eff effs (FuncDecl Out tv)
rnFuncDecl (Ps.MkBind binder body) = do
  name0 <- lookupGlobalName evarTab _DSign "undeclared function" binder
  let name1 = set namePos (getPos binder) name0
  MkFuncDecl name1 NoType <$> runReader @Env mempty (rnExpr body)

-- | Rename an external function declaration. The filled in type is bogus.
rnExtnDecl :: GlobalEffs effs => Ps.ExtnDecl -> Eff effs (ExtnDecl Out)
rnExtnDecl (Ps.MkExtnDecl binder extn) = do
  name0 <- lookupGlobalName evarTab _DSign "undeclared function" binder
  let name1 = set namePos (getPos binder) name0
  pure (MkExtnDecl name1 NoType extn)

-- | Rename a class declaration. Due to the FIXME for 'rnConstraints', all
-- constraints which methods put on the class type variables are silently
-- dropped on the floor.
rnClssDecl :: GlobalEffs effs => Ps.ClssDecl -> Eff effs ClassDecl
rnClssDecl (Ps.MkClssDecl name param0 super0 methods0) =
  introGlobal tconTab "type constructor" name DClss $ \clss -> do
    dcon <- mkName (fmap (retag . fmap ("." <>)) name)
    param1 <- mkName param0
    let env = Map.singleton (unlctd param0) param1
    super1 <- for super0 $ \(sname, stvar) -> do
      !_ <- lookupTyVar stvar env  -- make sure stvar and param0 are the same
      sclss <- lookupGlobalName tconTab _DClss "unknown class" sname
      (, sclss) <$> synthName (getPos clss) clss sclss
    let close = TUni' param1 . TCtx (clss, TVar param1)
    methods1 <- traverse (rnSignDecl env close) methods0
    pure (MkClassDecl clss param1 super1 dcon methods1)

-- | Rename an instance definition. There's /no/ check whether an instance for
-- this class/type combination has already been defined.
rnInstDecl :: GlobalEffs effs => Ps.InstDecl -> Eff effs (InstDecl Out)
rnInstDecl (Ps.MkInstDecl name0 clss0 tatom0 params0 ctxt0 methods0) = do
  clss1 <- lookupGlobal tconTab _DClss "unknown class" clss0
  tatom1 <- rnTypeAtom tatom0
  name1 <- mkName name0
  params1 <- traverse mkName params0
  let env = Map.fromList (zip (map unlctd params0) params1)
  ctxt1 <- rnConstraints env ctxt0
  ctxt2 <- for ctxt1 $ \(clss, tvar) ->
    (,) <$> mkDxVar clss tvar <*> pure (clss, TVar tvar)
  let super = fmap (const NoDict) (clss1 ^. class2super)
  MkInstDecl name1 (nameOf clss1) tatom1 params1 ctxt2 super <$> traverse rnFuncDecl methods0

-- | Rename an infix operator declaration, i.e., introduce the mapping from the
-- infix operator to the backing function to the global environment.
rnInfxDecl :: GlobalEffs effs => Ps.InfxDecl -> Eff effs ()
rnInfxDecl (Ps.MkInfxDecl (Lctd pos op) func) = do
  whenM (uses binopTab (Map.member op)) $
    throwAt pos ("duplicate operator" <:~> pretty op)
  bound <- lookupGlobalName evarTab _DSign "unknown function" func
  modifying binopTab (Map.insert op bound)

-- | Rename a top level declaration.
rnDecl :: GlobalEffs effs => Ps.Decl -> Eff effs (Maybe (Decl Out))
rnDecl = \case
  Ps.DType tcon -> Just . DType <$> rnTypeDecl tcon
  Ps.DSign sign -> Just . DSign <$> rnSignDecl mempty id sign
  Ps.DDefn defn -> Just . DFunc <$> rnFuncDecl defn
  Ps.DExtn extn -> Just . DExtn <$> rnExtnDecl extn
  Ps.DClss clss -> Just . DClss <$> rnClssDecl clss
  Ps.DInst inst -> Just . DInst <$> rnInstDecl inst
  Ps.DInfx infx -> Nothing      <$  rnInfxDecl infx

-- | Rename a whole package, i.e., a module with its transiticve dependencies.
renameModule :: Members [NameSource, Error Failure] effs =>
  Ps.Package -> Eff effs (Module Out)
renameModule (Ps.MkPackage _ modules) =
  let ldecls = concatMap Ps._mod2decls modules
  in  MkModule . catMaybes
      -- FIXME: Figure out the location of the declaration.
      <$> traverse (runReader noPos . rnDecl) ldecls
      & evalState (Tabs mempty mempty mempty mempty)
