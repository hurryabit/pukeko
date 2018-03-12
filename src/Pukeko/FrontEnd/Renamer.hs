{-# LANGUAGE ViewPatterns #-}
-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.FrontEnd.Renamer
  ( renameModule
  ) where

import Pukeko.Prelude

import           Control.Lens.Extras (is)
import           Control.Monad.Extra
import           Data.Bitraversable
import           Data.List.Extra   (nubOrd)
import qualified Data.Map.Extended as Map

import           Pukeko.AST.Name
import           Pukeko.AST.SystemF
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Surface    as Ps
import qualified Pukeko.AST.Operator   as Op
import           Pukeko.AST.Type

type Out = Surface

data Tabs = Tabs
  { _binopTab :: Map Op.Binary TmVar
  , _tconTab  :: Map (Ps.Name 'TyCon) (GenDecl (Only 'TyCon) Out)
  , _dconTab  :: Map (Ps.Name 'TmCon) TmConDecl
  , _evarTab  :: Map (Ps.Name 'TmVar) (GenDecl (Only 'TmVar) Out)
  }

type Env = Reader (Map (Ps.Name 'TmVar) TmVar)

type CanRn effs = Members [State Tabs, Reader SourcePos, NameSource, Error Failure] effs

type GlobalEffs effs = CanRn effs
-- type LocalEffs nsp effs = (GlobalEffs effs, Member (Reader (Env nsp)) effs)

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

lookupGlobal :: (GlobalEffs effs, NameSpaceOf decl ~ nsp, HasName decl) =>
  Lens' Tabs (Map (Ps.Name nsp) decl) ->
  (decl -> Bool) ->
  Failure ->
  Ps.LctdName nsp ->
  Eff effs (Name nsp)
lookupGlobal tab isok desc (Lctd pos name) =
  uses tab (Map.lookup name) >>= \case
    Nothing -> throwAt pos (desc <:~> pretty name)
    Just decl
      -- NOTE: 'rnClssDecl' is operating under the assumption that nobody ever
      -- looks into the non-binder field of a 'SignDecl'. Thus, we should /not/
      -- return the whole 'decl' until this is fixed.
      | isok decl -> pure (nameOf decl)
      | otherwise -> throwAt pos ("not a" <+> desc <:~> pretty name)

lookupBinop :: CanRn effs => Op.Binary -> Eff effs TmVar
lookupBinop op = do
  fun_mb <- uses binopTab (Map.lookup op)
  case fun_mb of
    Just fun -> pure fun
    Nothing  -> throwHere ("unknown operator:" <+> pretty op)

-- * Renaming of types

-- | Rename an atomic type. Does basically nothing unless it is a reference to a
-- type constructor.
rnTypeAtom :: GlobalEffs effs => Ps.TypeAtom -> Eff effs TypeAtom
rnTypeAtom = \case
  Ps.TAArr -> pure TAArr
  Ps.TAInt -> pure TAInt
  Ps.TACon name ->
    TACon <$> lookupGlobal tconTab (is _DType) "unknown type constructor" name

-- | Rename a type under the assumption that all its free variables are
-- contained in the map. All references to type constructors are renamed as
-- well.
rnType :: CanRn effs => Map (Ps.Name 'TyVar) TyVar -> Ps.Type -> Eff effs Type
rnType env = go
  where
    go = \case
      Ps.TVar (unlctd -> x)
        | Just v <- x `Map.lookup` env -> pure (TVar v)
        | otherwise -> throwHere ("unknown type variable:" <+> pretty x)
      Ps.TAtm a -> TAtm <$> rnTypeAtom a
      Ps.TApp tf tp -> TApp <$> go tf <*> go tp

-- | Collect all type class/type variable pairs in a type constraint. Rename all
-- references to type classes.
rnConstraints :: GlobalEffs effs =>
  Map (Ps.Name 'TyVar) TyVar -> Ps.TypeCstr -> Eff effs [TypeCstr]
rnConstraints env (Ps.MkTypeCstr constraints) =
  fmap (map (second TVar) . nubOrd) .
  for constraints $ \(clss, unlctd -> tvar0) ->
    case env Map.!? tvar0 of
      Nothing -> throwHere ("unknown type variable" <:~> pretty tvar0)
      Just tvar1 ->
        (, tvar1) <$> lookupGlobal tconTab (is _DClss) "unknown type class" clss

-- | Rename a type scheme. Fails if there are constraints on type variables
-- which are contained in the map or not in the type.
rnTypeScheme :: CanRn effs =>
  Map (Ps.Name 'TyVar) TyVar -> Ps.TypeScheme -> Eff effs Type
rnTypeScheme env0 (Ps.MkTypeScheme cstrs0 t0) = do
  env1 <- traverse mkName (Ps.freeTyVars t0 `Map.difference` env0)
  cstrs1 <- rnConstraints env1 cstrs0
  t1 <- rnType (env1 <> env0) t0
  pure (rewindr TUni' (Map.elems env1) (rewindr TCtx cstrs1 t1))

-- | Rename a type coercion.
rnCoercion :: GlobalEffs effs => Ps.Coercion -> Eff effs Coercion
rnCoercion (Ps.MkCoercion dir0 tname) = do
  let dir1 = case dir0 of
        Ps.Inject  -> Inject
        Ps.Project -> Project
  MkCoercion dir1 <$> lookupGlobal tconTab (is _DType) "unknown type constructor" tname


-- * Renaming of expressions

-- | Rename a pattern.
rnPatn :: CanRn effs =>
  Ps.Patn -> Eff effs (Patn Out, Map (Ps.Name 'TmVar) TmVar)
rnPatn = \case
  Ps.PWld -> pure (PWld, mempty)
  Ps.PVar name -> do
    binder <- mkName name
    pure (PVar (binder, NoType), Map.singleton (unlctd name) binder)
  Ps.PCon dcon0 patns0 -> do
    dcon1 <- lookupGlobal dconTab (const True) "unknown data constructor" dcon0
    (patns1, envs) <- unzip <$> traverse rnPatn patns0
    pure (PCon dcon1 patns1, fold envs)

-- | Rename a pattern match alternative.
rnAltn :: CanRn effs => Ps.Altn (Ps.LctdName 'TmVar) -> Eff (Env : effs) (Altn Out )
rnAltn (Ps.MkAltn patn0 rhs) = do
  (patn1, env) <- rnPatn patn0
  MkAltn patn1 <$> local (env `Map.union`) (rnExpr rhs)

-- | Rename a value abstraction (lambda).
rnELam :: CanRn effs =>
  [Ps.LctdName 'TmVar] -> Ps.Expr (Ps.LctdName 'TmVar) -> Eff (Env : effs) (Expr Out)
rnELam [] body = rnExpr body
rnELam (name:names) body = do
  binder <- mkName name
  ETmAbs (binder, NoType) <$> local (Map.insert (unlctd name) binder) (rnELam names body)

-- | Rename an expression.
rnExpr :: CanRn effs => Ps.Expr (Ps.LctdName 'TmVar) -> Eff (Env : effs) (Expr Out)
rnExpr = \case
  Ps.ELoc le -> here le $ ELoc <$> lctd rnExpr le
  Ps.EVar x -> do
    y_mb <- asks (unlctd x `Map.lookup`)
    case y_mb of
      Just y -> pure (EVar y)
      Nothing -> EVal <$> lookupGlobal evarTab (is _DSign) "unknown variable" x
  Ps.ECon name ->
    ECon <$> lookupGlobal dconTab (const True) "unknown data constructor" name
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
    names <- traverse mkName binders0
    let env = Map.fromList (zipExact (map unlctd binders0) names)
    exprs1 <- traverse rnExpr exprs0
    let binds1 = zipWithExact (\name expr -> MkBind (name, NoType) expr) names exprs1
    body1 <- local (env `Map.union`) (rnExpr body0)
    pure (ELet BindPar binds1 body1)
  Ps.ERec (toList -> binds0) body0 -> do
    let (binders0, exprs0) =
          unzip (map (\(Ps.MkBind binder expr) -> (binder, expr)) binds0)
    names <- traverse mkName binders0
    let env = Map.fromList (zipExact (map unlctd binders0) names)
    local (env `Map.union`) $ do
      exprs1 <- traverse rnExpr exprs0
      let binds1 =
            zipWithExact (\name expr -> MkBind (name, NoType) expr) names exprs1
      body1 <- rnExpr body0
      pure (ELet BindRec binds1 body1)
  Ps.ECoe c e -> ECast . (, NoType) <$> rnCoercion c <*> rnExpr e


-- * Renaming of top level declarations

-- | Rename a data constructor declaration. Assumes that the corresponding type
-- constructor (or at least some dummy version of it) has already been
-- introduced to the global environment.
rnTmConDecl :: CanRn effs =>
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
rnSignDecl :: CanRn effs =>
  Map (Ps.Name 'TyVar) TyVar -> (Type -> Type) -> Ps.SignDecl -> Eff effs (SignDecl tv)
rnSignDecl env preClose (Ps.MkSignDecl binder typeScheme) =
  introGlobal evarTab "function declaration" binder mkDSign $ \name ->
    MkSignDecl name <$> rnTypeScheme env typeScheme
  where
    mkDSign = DSign . over sign2type preClose

-- | Rename a function declaration. The filled in type is bogus. For top level
-- functions, we have @tv = Void@. For method definitions, we have @tv = TScope
-- Int Void@.
rnFuncDecl :: CanRn effs => Ps.Bind (Ps.LctdName 'TmVar) -> Eff effs (FuncDecl Out tv)
rnFuncDecl (Ps.MkBind binder body) = do
  name0 <- lookupGlobal evarTab (is _DSign) "undeclared function" binder
  let name1 = set namePos (getPos binder) name0
  MkFuncDecl name1 NoType <$> runReader mempty (rnExpr body)

-- | Rename an external function declaration. The filled in type is bogus.
rnExtnDecl :: CanRn effs => Ps.ExtnDecl -> Eff effs (ExtnDecl Out)
rnExtnDecl (Ps.MkExtnDecl binder extn) = do
  name0 <- lookupGlobal evarTab (is _DSign) "undeclared function" binder
  let name1 = set namePos (getPos binder) name0
  MkExtnDecl name1 NoType <$> pure extn

-- | Rename a class declaration. Due to the FIXME for 'rnConstraints', all
-- constraints which methods put on the class type variables are silently
-- dropped on the floor.
rnClssDecl :: GlobalEffs effs => Ps.ClssDecl -> Eff effs ClassDecl
rnClssDecl (Ps.MkClssDecl name param0 methods0) =
  introGlobal tconTab "type constructor" name DClss $ \clss -> do
    dcon <- mkName (fmap (retag . fmap ("." <>)) name)
    param1 <- mkName param0
    let env = Map.singleton (unlctd param0) param1
    let close = TUni' param1 . TCtx (clss, TVar param1)
    methods1 <- traverse (rnSignDecl env close) methods0
    pure (MkClassDecl clss param1 dcon methods1)

-- | Rename an instance definition. There's /no/ check whether an instance for
-- this class/type combination has already been defined.
rnInstDecl :: GlobalEffs effs => Ps.InstDecl -> Eff effs (InstDecl Out)
rnInstDecl (Ps.MkInstDecl name0 clss0 tatom0 params0 cstrs0 methods0) = do
  clss1 <- lookupGlobal tconTab (is _DClss) "unknown class" clss0
  tatom1 <- rnTypeAtom tatom0
  name1 <- mkName name0
  params1 <- traverse mkName params0
  let env = Map.fromList (zip (map unlctd params0) params1)
  cstrs1 <- rnConstraints env cstrs0
  MkInstDecl name1 clss1 tatom1 params1 cstrs1 <$> traverse rnFuncDecl methods0

-- | Rename an infix operator declaration, i.e., introduce the mapping from the
-- infix operator to the backing function to the global environment.
rnInfxDecl :: GlobalEffs effs => Ps.InfxDecl -> Eff effs ()
rnInfxDecl (Ps.MkInfxDecl (Lctd pos op) func) = do
  whenM (uses binopTab (Map.member op)) $
    throwAt pos ("duplicate operator" <:~> pretty op)
  bound <- lookupGlobal evarTab (is _DSign) "unknown function" func
  modifying binopTab (Map.insert op bound)

-- | Rename a top level declaration.
rnDecl :: CanRn effs => Ps.Decl -> Eff effs (Maybe (Decl Out))
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
