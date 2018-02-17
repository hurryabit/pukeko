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

renameModule :: Member (Error Failure) effs =>
  Ps.Package -> Eff effs (Module Out)
renameModule (Ps.MkPackage _ modules) =
  runNameSource . runReader noPos . evalState st0 $ do
    let ldecls = concatMap Ps._mod2decls modules
    MkModule . catMaybes <$> traverse rnDecl ldecls
  where
    st0 = Tabs mempty mempty mempty

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

rnDecl :: CanRn effs => Ps.Decl -> Eff effs (Maybe (Decl Out))
rnDecl = \case
  Ps.DType t -> Just . DType <$> rnTypeDecl t
  Ps.DSign s -> Just . DSign <$> rnSignDecl mempty s
  Ps.DClss c -> Just . DClss <$> rnClssDecl c
  Ps.DInst i -> Just . DInst <$> rnInstDecl i
  Ps.DDefn d -> Just . DDefn <$> runReader mempty (rnDefn d)
  Ps.DExtn (Ps.MkExtnDecl z s) -> do
    modifying funcs (<> Set.singleton (z^.lctd))
    yield (DExtn (MkExtnDecl (MkBind z NoType) s))
  Ps.DInfx (Ps.MkInfxDecl l@(unlctd -> op) fun) -> here l $ do
    checkFunc fun
    modifying binops (Map.insert op fun)
    pure Nothing
  where
    yield = pure . Just

-- -- | Rename a 'Ps.DConDecl' under the assumption that all mentioned type
-- -- constructors are already in the global environment.
-- rnDConDecl :: LocalEffs TVar effs =>
--   Binder TCon -> Int -> Ps.DConDecl -> Eff effs DConDecl
-- rnDConDecl tcon tag (Ps.MkDConDecl name fields0) =
--   introGlobal dconTab "data constructor" name id $ \binder ->
--     MkDConDecl (bindAt name tcon) binder tag <$> traverse rnType fields0

-- | Rename a (potentially) recursive 'PS.TConDecl'.
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

rnDConDecl :: CanRn effs =>
  Name TCon -> [Id.TVar] -> Int -> Ps.DConDecl -> Eff effs DConDecl
rnDConDecl tcon vs tag (Ps.MkDConDecl dcon flds) = here dcon $
  MkDConDecl tcon dcon tag <$> traverse (rnType env) flds
  where
    env = finRenamer vs

rnSignDecl :: CanRn effs => Map Id.TVar tv -> Ps.SignDecl -> Eff effs (Bind Type tv)
rnSignDecl env (Ps.MkSignDecl z t) = do
  modifying funcs (Set.insert (z^.lctd))
  MkBind z <$> rnTypeScheme env t

-- | Rename a 'Ps.ClssDecl'. Due to the FIXME for 'rnConstraints', all
-- constraints methods put on the class type variables are silently dropped on
-- the floor.
rnClssDecl :: GlobalEffs effs => Ps.ClssDecl -> Eff effs ClssDecl
rnClssDecl (Ps.MkClssDecl name param methods0) =
  introGlobal tconTab "type constructor" name Right $ \binder -> do
    let env = Map.singleton param (mkBound 0 param)
    methods1 <- traverse (rnSignDecl env) methods0
    modifying funcs (<> setOf (traverse . bind2evar . lctd) methods1)
    pure (MkClssDecl binder param methods1)


-- | Rename an instance definition. There's /no/ check whether an instance for
-- this class/type combination has already been defined.
rnInstDecl :: GlobalEffs effs => Ps.InstDecl -> Eff effs (InstDecl Out)
rnInstDecl (Ps.MkInstDecl clss0 tatm0 params0 cstrs0 defns0) = do
  cstrs1 <- rnConstraints cstrs0
  MkInstDecl
    <$> lookupGlobal tconTab isRight "class" clss0
    <*> rnTypeAtom tatm0
    <*> applyConstraints params0 cstrs1
    <*> traverse (runReader mempty . rnDefn) defns0

rnTypeAtom :: GlobalEffs effs => Ps.TypeAtom -> Eff effs TypeAtom
rnTypeAtom = \case
  Ps.TAArr -> pure TAArr
  Ps.TAInt -> pure TAInt
  Ps.TACon name -> TACon <$> lookupGlobal tconTab isLeft "type constructor" name

rnType :: CanRn effs => Map Id.TVar tv -> Ps.Type -> Eff effs (Type tv)
rnType env = go
  where
    go = \case
      Ps.TVar x
        | Just v <- x `Map.lookup` env -> pure (TVar v)
        | otherwise -> throwHere ("unknown type variable:" <+> pretty x)
      Ps.TAtm a -> TAtm <$> rnTypeAtom a
      Ps.TApp tf tp -> TApp <$> go tf <*> go tp

-- | Rename a 'Ps.TypeCstr'.
rnConstraints :: GlobalEffs effs =>
  Ps.TypeCstr -> Eff effs (Map Id.TVar (Set (Name Clss)))
rnConstraints (Ps.MkTypeCstr constraints) =
  fmap (fmap Set.fromList . Map.fromMultiList) . for constraints $ \(clss, tvar) ->
    (,) tvar <$> lookupGlobal tconTab isRight "type class" clss

rnTypeScheme :: CanRn effs => Map Id.TVar tv -> Ps.TypeScheme -> Eff effs (Type tv)
rnTypeScheme env (Ps.MkTypeScheme qs0 t) = do
  let vs0 = toList (setOf Ps.type2tvar t `Set.difference` Map.keysSet env)
  qs1 <- rnConstraints qs0
  qvs <- applyConstraints vs0 qs1
  mkTUni qvs <$> rnType (fmap weakenScope env <> finRenamer vs0) t

rnCoercion :: GlobalEffs effs => Ps.Coercion -> Eff effs Coercion
rnCoercion (Ps.MkCoercion dir0 tname) = do
  let dir1 = case dir0 of
        Ps.Inject  -> Inject
        Ps.Project -> Project
  MkCoercion dir1 <$> lookupGlobal tconTab isLeft "type constructor" tname

rnDefn :: CanRn effs => Ps.Defn Id.EVar -> Eff (Env ev : effs) (Defn Out tv ev)
rnDefn (Ps.MkDefn b e) = MkDefn (rnBind b) <$> rnExpr e

rnELam :: CanRn effs =>
  [Lctd Id.EVar] -> Ps.Expr Id.EVar -> Eff (Env ev : effs) (Expr Out tv ev)
rnELam [] e0 = rnExpr e0
rnELam (b0:bs) e0 = do
  let b1@(MkBind (unlctd -> x) NoType) = rnBind b0
  ELam b1 <$> localize1 x (rnELam bs e0)

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

rnBind :: Lctd Id.EVar -> Bind NoType tv
rnBind x = MkBind x NoType

rnAltn :: CanRn effs => Ps.Altn Id.EVar -> Eff (Env ev : effs) (Altn Out tv ev)
rnAltn (Ps.MkAltn p0 e) = do
  let p1 = rnPatn p0
  let bs = Map.fromSet id (setOf patn2evar p1)
  MkAltn p1 <$> localize bs (rnExpr e)

rnPatn :: Ps.Patn -> Patn Out tv
rnPatn = \case
  Ps.PWld      -> PWld
  Ps.PVar x    -> PVar x
  Ps.PCon c ps -> PCon c [] (map rnPatn ps)
