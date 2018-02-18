{-# LANGUAGE ViewPatterns #-}
-- | Transform AST to use type safe de Bruijn indices.
module Pukeko.FrontEnd.Renamer
  ( renameModule
  ) where

import Pukeko.Prelude

import           Control.Lens.Indexed
import           Control.Monad.Extra
import           Data.Bitraversable
import           Data.Either       (isLeft, isRight)
import qualified Data.Map.Extended as Map
import qualified Data.Set          as Set

import           Pukeko.AST.Name
import           Pukeko.AST.SystemF
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Surface    as Ps
import qualified Pukeko.AST.Identifier as Id
import qualified Pukeko.AST.Operator   as Op
import           Pukeko.AST.Type

type Out = Surface

data Tabs = Tabs
  { _funcs  :: Set Id.EVar
  , _binops :: Map Op.Binary Id.EVar
  , _tconTab :: Map (Ps.Name TCon) (Either TConDecl ClssDecl)
  }

type Env ev = Reader (Map Id.EVar ev)

type CanRn effs = Members [State Tabs, Reader SourcePos, NameSource, Error Failure] effs

type GlobalEffs effs = CanRn effs
-- type LocalEffs nsp effs = (GlobalEffs effs, Member (Reader (Env nsp)) effs)

makeLenses ''Tabs

-- | Create a new global 'Binder' if the name has not been declared before in
-- the same namespace and pass the result to the continuation. The continuations
-- result is finally stored in the declaration table.
introGlobal :: GlobalEffs effs =>
  (Lens' Tabs (Map (Ps.Name nsp) decl)) ->
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
  (Lens' Tabs (Map (Ps.Name nsp) decl)) ->
  (decl -> Bool) ->
  Failure ->
  Ps.LctdName nsp ->
  Eff effs (Name nsp)
lookupGlobal tab isok desc (Lctd pos name) = do
  uses tab (Map.lookup name) >>= \case
    Nothing -> throwAt pos ("unknown" <+> desc <:~> pretty name)
    Just decl
      -- NOTE: 'rnClssDecl' is operating under the assumption that nobody ever
      -- looks into the non-binder field of a 'SignDecl'. Thus, we should /not/
      -- return the whole 'decl' until this is fixed.
      | isok decl -> pure (nameOf decl)
      | otherwise -> throwAt pos ("not a" <+> desc <:~> pretty name)


localize :: CanRn effs =>
  Map Id.EVar i -> Eff (Env (EScope i ev) : effs) a -> Eff (Env ev : effs) a
localize bs = local' upd
  where
    upd env = Map.mapWithKey (flip mkBound) bs `Map.union` Map.map Free env

localize1 :: CanRn effs =>
  Id.EVar -> Eff (Env (EScope () ev) : effs) a -> Eff (Env ev : effs) a
localize1 x = localize (Map.singleton x ())

-- TODO: Make @\(Ps.MkDefn x _) -> x@ a function and use it.
localizeDefns :: (CanRn effs, FoldableWithIndex Int t) =>
  t (Ps.Defn _) -> Eff (Env (EScope Int ev) : effs) a -> Eff (Env ev : effs) a
localizeDefns =
  localize . ifoldMap (\i (Ps.MkDefn (unlctd -> x) _) -> Map.singleton x i)

checkFunc :: CanRn effs => Id.EVar -> Eff effs ()
checkFunc z = do
  global <- uses funcs (Set.member z)
  unless global (throwHere ("unknown variable:" <+> pretty z))

findBinop :: CanRn effs => Op.Binary -> Eff effs Id.EVar
findBinop op = do
  fun_mb <- uses binops (Map.lookup op)
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
  Ps.TACon name -> TACon <$> lookupGlobal tconTab isLeft "type constructor" name

-- | Rename a type under the assumption that all its free variables are
-- contained in the map. All references to type constructors are renamed as
-- well.
rnType :: CanRn effs => Map Id.TVar tv -> Ps.Type -> Eff effs (Type tv)
rnType env = go
  where
    go = \case
      Ps.TVar x
        | Just v <- x `Map.lookup` env -> pure (TVar v)
        | otherwise -> throwHere ("unknown type variable:" <+> pretty x)
      Ps.TAtm a -> TAtm <$> rnTypeAtom a
      Ps.TApp tf tp -> TApp <$> go tf <*> go tp

-- | Collect all type class-type variable pairs in a type constraint. Rename all
-- references to type classes.
rnConstraints :: GlobalEffs effs =>
  Ps.TypeCstr -> Eff effs (Map Id.TVar (Set (Name Clss)))
rnConstraints (Ps.MkTypeCstr constraints) =
  fmap (fmap Set.fromList . Map.fromMultiList) . for constraints $ \(clss, tvar) ->
    (,) tvar <$> lookupGlobal tconTab isRight "type class" clss

-- | Rename a type scheme. Fails if there are constraints on type variables
-- which are contained in the map or not in the type.
rnTypeScheme :: CanRn effs => Map Id.TVar tv -> Ps.TypeScheme -> Eff effs (Type tv)
rnTypeScheme env (Ps.MkTypeScheme qs0 t) = do
  let vs0 = toList (setOf Ps.type2tvar t `Set.difference` Map.keysSet env)
  qs1 <- rnConstraints qs0
  qvs <- applyConstraints vs0 qs1
  mkTUni qvs <$> rnType (fmap weakenScope env <> finRenamer vs0) t

-- | Rename a type coercion.
rnCoercion :: GlobalEffs effs => Ps.Coercion -> Eff effs Coercion
rnCoercion (Ps.MkCoercion dir0 tname) = do
  let dir1 = case dir0 of
        Ps.Inject  -> Inject
        Ps.Project -> Project
  MkCoercion dir1 <$> lookupGlobal tconTab isLeft "type constructor" tname


-- * Renaming of expressions

-- | Rename a typed binder.
rnBind :: Lctd Id.EVar -> Bind NoType tv
rnBind x = MkBind x NoType

-- | Rename a pattern.
rnPatn :: Ps.Patn -> Patn Out tv
rnPatn = \case
  Ps.PWld      -> PWld
  Ps.PVar x    -> PVar x
  Ps.PCon c ps -> PCon c [] (map rnPatn ps)

-- | Rename a definition.
rnDefn :: CanRn effs => Ps.Defn Id.EVar -> Eff (Env ev : effs) (Defn Out tv ev)
rnDefn (Ps.MkDefn b e) = MkDefn (rnBind b) <$> rnExpr e

-- | Rename a pattern match alternative.
rnAltn :: CanRn effs => Ps.Altn Id.EVar -> Eff (Env ev : effs) (Altn Out tv ev)
rnAltn (Ps.MkAltn p0 e) = do
  let p1 = rnPatn p0
  let bs = Map.fromSet id (setOf patn2evar p1)
  MkAltn p1 <$> localize bs (rnExpr e)

-- | Rename a value abstraction (lambda).
rnELam :: CanRn effs =>
  [Lctd Id.EVar] -> Ps.Expr Id.EVar -> Eff (Env ev : effs) (Expr Out tv ev)
rnELam [] e0 = rnExpr e0
rnELam (b0:bs) e0 = do
  let b1@(MkBind (unlctd -> x) NoType) = rnBind b0
  ELam b1 <$> localize1 x (rnELam bs e0)

-- | Rename an expression.
rnExpr :: CanRn effs => Ps.Expr Id.EVar -> Eff (Env ev : effs) (Expr Out tv ev)
rnExpr = \case
  Ps.ELoc le -> here le $ ELoc <$> lctd rnExpr le
  Ps.EVar x -> do
    y_mb <- asks (x `Map.lookup`)
    case y_mb of
      Just y -> pure (EVar y)
      Nothing -> do
        checkFunc x
        pure (EVal x)
  Ps.ECon c -> pure (ECon c)
  Ps.ENum n -> pure (ENum n)
  Ps.EApp e0 es -> foldl EApp <$> rnExpr e0 <*> traverse rnExpr es
  Ps.EOpp op e1 e2 ->
    EApp <$> (EApp <$> (EVal <$> findBinop op) <*> rnExpr e1) <*> rnExpr e2
  Ps.EMat e0 as0 ->
    case as0 of
      []   -> throwHere "pattern match without alternatives"
      a:as -> EMat <$> rnExpr e0 <*> traverse rnAltn (a :| as)
  Ps.ELam (toList -> bs0) e0 -> rnELam bs0 e0
  Ps.ELet (toList -> ds0) e0 ->
    ELet <$> traverse rnDefn ds0 <*> localizeDefns ds0 (rnExpr e0)
  Ps.ERec (toList -> ds0) e0 ->
    localizeDefns ds0 $ ERec <$> traverse rnDefn ds0 <*> rnExpr e0
  Ps.ECoe c e -> ETyCoe <$> rnCoercion c <*> rnExpr e


-- * Renaming of top level declarations

-- | Rename a data constructor declaration. Assumes that the corresponding type
-- constructor (or at least some dummy version of it) has already been
-- introduced to the global environment.
rnDConDecl :: CanRn effs =>
  Name TCon -> [Id.TVar] -> Int -> Ps.DConDecl -> Eff effs DConDecl
rnDConDecl tcon vs tag (Ps.MkDConDecl dcon flds) = here dcon $
  MkDConDecl tcon dcon tag <$> traverse (rnType env) flds
  where
    env = finRenamer vs

-- | Rename a (potentially) recursive type declaration.
rnTypeDecl :: GlobalEffs effs => Ps.TConDecl -> Eff effs TConDecl
rnTypeDecl (Ps.MkTConDecl name params0 dcons0) = do
  -- NOTE: Since the type definition might be recursive, we need to introduce a
  -- dummy version of the type constructor first and overwrite it in the end.
  let mkDummy binder = Left (MkTConDecl binder [] (Right []))
  binder <- introGlobal tconTab "type constructor" name mkDummy pure
  -- (dcons1, params1) <- runReader @(Env TVar) mempty $
  --   introMany (introLocal "type parameter") params0 $
  --     bitraverse rnType (itraverse (rnDConDecl binder)) dcons0
  dcons1 <- bitraverse
            (rnType (finRenamer params0))
            (zipWithM (\tag -> rnDConDecl binder params0 tag) [0..])
            dcons0
  let tcon = MkTConDecl binder params0 dcons1
  modifying tconTab (Map.insert (unlctd name) (Left tcon))
  pure tcon

-- | Rename a signature declaration. For signatures of top level functions, we
-- have @tv = Void@ and hence the map is empty. For method signatures, we have
-- @tv = TScope Int Void@ and the map contains the class paramaters.
rnSignDecl :: CanRn effs => Map Id.TVar tv -> Ps.SignDecl -> Eff effs (SignDecl tv)
rnSignDecl env (Ps.MkSignDecl z t) = do
  modifying funcs (Set.insert (z^.lctd))
  MkSignDecl z <$> rnTypeScheme env t

-- | Rename a function declaration. The filled in type is bogus. For top level
-- functions, we have @tv = Void@. For method definitions, we have @tv = TScope
-- Int Void@.
rnFuncDecl :: CanRn effs => Ps.Defn Id.EVar -> Eff effs (FuncDecl Out tv)
rnFuncDecl (Ps.MkDefn func body) =
  MkFuncDecl func NoType <$> runReader mempty (rnExpr body)

-- | Rename an external function declaration. The filled in type is bogus.
rnExtnDecl :: CanRn effs => Ps.ExtnDecl -> Eff effs (ExtnDecl Out)
rnExtnDecl (Ps.MkExtnDecl func extn) = pure (MkExtnDecl func NoType extn)

-- | Rename a class declaration. Due to the FIXME for 'rnConstraints', all
-- constraints which methods put on the class type variables are silently
-- dropped on the floor.
rnClssDecl :: GlobalEffs effs => Ps.ClssDecl -> Eff effs ClssDecl
rnClssDecl (Ps.MkClssDecl name param methods0) =
  introGlobal tconTab "type constructor" name Right $ \binder -> do
    let env = Map.singleton param (mkBound 0 param)
    methods1 <- for methods0 $ \mthd@(Ps.MkSignDecl z _) -> do
      modifying funcs (Set.insert (z^.lctd))
      rnSignDecl env mthd
    pure (MkClssDecl binder param methods1)

-- | Rename an instance definition. There's /no/ check whether an instance for
-- this class/type combination has already been defined.
rnInstDecl :: GlobalEffs effs => Ps.InstDecl -> Eff effs (InstDecl Out)
rnInstDecl (Ps.MkInstDecl clss0 tatm0 params0 cstrs0 methods0) = do
  cstrs1 <- rnConstraints cstrs0
  MkInstDecl
    <$> lookupGlobal tconTab isRight "class" clss0
    <*> rnTypeAtom tatm0
    <*> applyConstraints params0 cstrs1
    <*> traverse rnFuncDecl methods0

-- | Rename an infix operator declaration, i.e., introduce the mapping from the
-- infix operator to the backing function to the global environment.
rnInfxDecl :: GlobalEffs effs => Ps.InfxDecl -> Eff effs ()
rnInfxDecl (Ps.MkInfxDecl l@(unlctd -> op) fun) = here l $ do
    checkFunc fun
    modifying binops (Map.insert op fun)

-- | Rename a top level declaration.
rnDecl :: CanRn effs => Ps.Decl -> Eff effs (Maybe (Decl Out))
rnDecl = \case
  Ps.DType tcon -> Just . DType <$> rnTypeDecl tcon
  Ps.DSign sign -> Just . DSign <$> rnSignDecl mempty sign
  Ps.DDefn defn -> Just . DFunc <$> rnFuncDecl defn
  Ps.DExtn extn -> Just . DExtn <$> rnExtnDecl extn
  Ps.DClss clss -> Just . DClss <$> rnClssDecl clss
  Ps.DInst inst -> Just . DInst <$> rnInstDecl inst
  Ps.DInfx infx -> Nothing      <$  rnInfxDecl infx

-- | Rename a whole package, i.e., a module with its transiticve dependencies.
renameModule :: Member (Error Failure) effs => Ps.Package -> Eff effs (Module Out)
renameModule (Ps.MkPackage _ modules) =
  let ldecls = concatMap Ps._mod2decls modules
  in  MkModule . catMaybes <$> traverse rnDecl ldecls
      & evalState (Tabs mempty mempty mempty)
      & runReader noPos
      & runNameSource
