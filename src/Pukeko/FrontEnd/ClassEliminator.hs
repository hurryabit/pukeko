module Pukeko.FrontEnd.ClassEliminator
  ( elimModule
  ) where

import Pukeko.Prelude

import qualified Data.List.NE      as NE
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Vector       as Vec
import qualified Safe.Exact        as Safe

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.ConDecl
import           Pukeko.AST.SystemF
import           Pukeko.AST.Language
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Info
import           Pukeko.FrontEnd.Gamma

type In  = Unnested
type Out = Unclassy

type IsTVar tv = (BaseTVar tv, HasEnv tv, Show tv)

type CE tv ev =
  EffXGamma (Map Id.Clss) Type tv ev [Reader ModuleInfo, Reader SourcePos]

runCE :: Module In -> CE Void Void a -> a
runCE m0 = run . runReader noPos . runInfo m0 . runGamma

elimModule :: Module In -> Module Out
elimModule m0@(MkModule decls) = runCE m0 $
  MkModule . map unclssDecl . concat <$> traverse elimDecl decls

-- | Name of the dictionary type constructor of a type class, e.g., @Dict$Eq@
-- for type class @Eq@.
clssTCon :: Id.Clss -> Id.TCon
clssTCon clss = Id.tcon ("Dict$" ++ Id.name clss)

-- | Name of the dictionary data constructor of a type class, e.g., @Dict$Eq@
-- for type class @Eq@. This is currently the same as the dictionary type
-- constructor.
clssDCon :: Id.Clss -> Id.DCon
clssDCon clss = Id.dcon ("Dict$" ++ Id.name clss)

-- | Name of the dictionary for a type class instance of either a known type
-- ('Id.TCon') or an unknown type ('Id.TVar'), e.g., @dict@Traversable$List@ or
-- @dict$Monoid$m@.
dictEVar :: Id.Clss -> Either Id.TCon Id.TVar -> Id.EVar
dictEVar clss tcon =
  Id.evar ("dict$" ++ Id.name clss ++ "$" ++ either Id.name Id.name tcon)

-- | Apply the dictionary type constructor of a type class to a type. For the
-- @List@ instance of @Traversable@, we obtain @Dict$Traversable List$, i.e.,
--
-- > TApp (TCon "Dict$Traversable") [TCon "List"]
mkTDict :: Id.Clss -> Type tv -> Type tv
mkTDict clss t = mkTApp (TCon (clssTCon clss)) [t]

-- | Get the name of the dictionary for a type class instance and its type.
--
-- The @List@ instance of @Traversable@ yields
--
-- > dict$Traversable$List : Dict$Traversable List
--
-- The @List@ instance of @Eq@ yields
--
-- > dict$Eq$List : ∀a. (Eq a) => Dict$Eq (List a)
instDictInfo :: InstDecl st -> (Id.EVar, Type Void)
instDictInfo (MkInstDecl (unlctd -> clss) tcon qvs _) =
    let t_dict = mkTUni qvs (mkTDict clss (mkTApp (TCon tcon) (mkTVarsQ qvs)))
    in  (dictEVar clss (Left tcon), t_dict)

-- | Construct the dictionary data type declaration of a type class declaration.
-- See 'elimClssDecl' for an example.
dictTConDecl :: ClssDecl -> TConDecl
dictTConDecl (MkClssDecl (Lctd pos clss) prm mthds) =
    let flds = map _bind2type mthds
        dcon = MkDConDecl (clssTCon clss) (Lctd pos (clssDCon clss)) 0 flds
    in  MkTConDecl (Lctd pos (clssTCon clss)) [prm] (Right [dcon])

-- | Transform a type class declaration into a data type declaration for the
-- dictionary and projections from the dictionary to each class method.
--
-- The @Traversable@ class is transformed as follows, e.g.:
--
-- > class Traversable t where
-- >   traverse : (Monad m) => (a -> m b) -> t a -> m (t b)
--
-- is turned into
--
-- > data Dict$Traversable t =
-- >   | Dict$Traversable (∀a b m. Dict$Monad m -> (a -> m b) -> t a -> m (t b))
-- > traverse : ∀t. Dict$Traversable t
-- >         -> (∀a b m. Dict$Monad m -> (a -> m b) -> t a -> m (t b)) =
-- >   fun @t ->
-- >     fun (dict : Dict$Traversable t) ->
-- >       match dict with
-- >       | Dict$Traversable @t traverse -> traverse
elimClssDecl :: ClssDecl -> CE Void Void [Decl Out]
elimClssDecl clssDecl@(MkClssDecl (unlctd -> clss) prm mthds) = do
  let tcon = dictTConDecl clssDecl
  let qprm = NE.singleton (MkQVar mempty prm)
      prmType = TVar (mkBound 0 prm)
      dictPrm = Id.evar "dict"
      sels = do
        (i, MkBind (Lctd mpos z) t0) <- itoList mthds
        let t1 = TUni (NE.singleton (MkQVar (Set.singleton clss) prm)) t0
        let e_rhs = EVar (mkBound z z)
        let c_binds = imap (\j _ -> guard (i==j) *> pure z) mthds
        let c_one = MkAltn (PSimple (clssDCon clss) [prmType] c_binds) e_rhs
        let e_cas = EMat (EVar (mkBound () dictPrm)) (c_one :| [])
        let b_lam = MkBind (Lctd mpos dictPrm) (mkTDict clss prmType)
        let e_lam = ELam b_lam (ETyAnn t0 e_cas)
        let e_tyabs = ETyAbs qprm e_lam
        pure (DDefn (MkDefn (MkBind (Lctd mpos z) t1) e_tyabs))
  pure (DType (NE.singleton tcon) : sels)

-- | Transform a class instance definition into a dictionary definition.
--
-- The @List@ instance of @Traversable@ is transformed as follows, e.g.:
--
-- > instance Traversable List where
-- >   traverse f xs = sequence (map f xs)
--
-- is transformed into
--
-- > dict$Traversable$List : Dict$Traversable List =
-- >   let traverse : ∀a b m. (Monad m) => (a -> m b) -> List a -> m (List b) =
-- >     fun @a @b @(m ∈ Monad) ->
-- >       fun (f : a -> m b) (xs : List a) ->
-- >         sequence @b @m (map @List @a @(m b) f xs)
-- >   in
-- >   Dict$Traversable @List traverse
elimInstDecl :: ClssDecl -> InstDecl In -> CE Void Void [Decl In]
elimInstDecl clssDecl inst@(MkInstDecl (Lctd ipos clss) tcon qvs defns0) = do
  let t_inst = mkTApp (TCon tcon) (mkTVarsQ qvs)
  let (z_dict, t_dict) = instDictInfo inst
  defns1 <- for (clssDecl^.clss2mthds) $ \(MkBind (unlctd -> mthd) _) -> do
    case find (\defn -> defn^.defn2func == mthd) defns0 of
      Nothing -> bugWith "missing method" (clss, tcon, mthd)
      Just defn -> pure defn
  let e_dcon = mkETyApp (ECon (clssDCon clss)) [t_inst]
  let e_body =
        foldl EApp e_dcon (imap (\i -> EVar . mkBound i . (^.defn2func)) defns1)
  let e_let :: Expr In _ _
      e_let = ELet defns1 e_body
  let e_rhs :: Expr In _ _
      e_rhs = mkETyAbs qvs e_let
  pure [DDefn (MkDefn (MkBind (Lctd ipos z_dict) t_dict) e_rhs)]

elimDecl :: Decl In -> CE Void Void [Decl Out]
elimDecl = here' $ \case
  DClss clss  -> elimClssDecl clss
  DInst inst  -> do
    -- TODO: Constructing the declaration of the dictionary type again and
    -- putting it in the environment locally is a bit a of a hack.
    clss <- findInfo info2clsss (inst^.inst2clss.lctd)
    local (<> tconDeclInfo (dictTConDecl clss)) $ do
      defns <- elimInstDecl clss inst
      concat <$> traverse elimDecl defns
  DDefn defn  -> (:[]) . DDefn <$> elimDefn defn
  DType tcons -> pure [DType tcons]
  DExtn extn  -> pure [DExtn extn]

buildDict :: (IsTVar tv) => Id.Clss -> Type tv -> CE tv ev (Expr Out tv ev)
buildDict clss t0 = do
  let (t1, tps) = gatherTApp t0
  case t1 of
    TVar v
      | null tps -> lookupTVar v >>= maybe bugNoDict (pure . EVar) . Map.lookup clss
    TCon tcon -> do
      SomeInstDecl inst <- findInfo info2insts (clss, tcon)
      let (z_dict, t_dict) = instDictInfo inst
      fst <$> elimETyApp (EVal z_dict) (fmap absurd t_dict) tps
    _ -> bugNoDict
  where
    bugNoDict = bugWith "cannot build dict" (clss, t0)

elimETyApp ::
  (IsTVar tv) =>
  Expr Out tv ev -> Type tv -> [Type tv] -> CE tv ev (Expr Out tv ev, Type tv)
elimETyApp e0 t_e0 ts0 = do
    let (qvs, t_e1) = gatherTUni t_e0
    let dictBldrs = do
          (MkQVar qual _, t1) <- toList (zip qvs ts0)
          clss <- toList qual
          pure (buildDict clss t1)
    dicts <- sequence dictBldrs
    pure (foldl EApp (mkETyApp e0 ts0) dicts, instantiateN ts0 t_e1)

-- | Enrich a type abstraction with value abstractions for type class
-- dictionaries.
elimETyAbs :: (IsTVar tv, HasEnv ev) =>
  NonEmpty QVar -> Expr In (TScope Int tv) ev -> CE tv ev (Expr Out tv ev, Type tv)
elimETyAbs qvs0 e0 = do
    pos <- where_
    let ixbs = do
          (i, MkQVar qual v) <- itoList qvs0
          clss <- toList qual
          let x = dictEVar clss (Right v)
          pure ((i, clss, x), MkBind (Lctd pos x) (mkTDict clss (TVar (mkBound i v))))
    let (ixs, bs) = unzip ixbs
    let refs = Vec.accum Map.union (Vec.replicate (length qvs0) mempty)
               [ (i, Map.singleton clss (mkBound j x))
               | (j, (i, clss, x)) <- itoList ixs
               ]
    (e1, t1) <-
      withinXScope refs (Vec.fromList (fmap _bind2type bs)) (elimExpr (weakenE e0))
    let qvs1 = fmap (qvar2cstr .~ mempty) qvs0
    pure (ETyAbs qvs1 (mkELam bs t1 e1), TUni qvs0 t1)

elimDefn :: (IsTVar tv, HasEnv ev) => Defn In tv ev -> CE tv ev (Defn Out tv ev)
elimDefn = defn2expr (fmap fst . elimExpr)

-- TODO: Figure out if we can remove the 'Type' component from the result.
-- Adding type annotations is perhaps a better way to solve this problem.
elimExpr ::
  (IsTVar tv, HasEnv ev) => Expr In tv ev -> CE tv ev (Expr Out tv ev, Type tv)
elimExpr = \case
  ELoc (Lctd pos e0) -> here_ pos $ first (ELoc . Lctd pos) <$> elimExpr e0
  EVar x -> (,) <$> pure (EVar x) <*> lookupEVar x
  EAtm a -> (,) <$> pure (EAtm a) <*> typeOfAtom a
  EApp fun0 arg0 -> do
    (fun1,  t_fun) <- elimExpr fun0
    (arg1, _t_arg) <- elimExpr arg0
    case t_fun of
      TFun _ t_res -> pure (EApp fun1 arg1, t_res)
      _            -> bug "too many arguments"
  ELam param body0 -> do
    let t_param = _bind2type param
    (body1, t_body) <- withinEScope1 id t_param (elimExpr body0)
    pure (ELam param body1, t_param ~> t_body)
  ELet ds0 e0 -> do
    ds1 <- traverse elimDefn ds0
    (e1, t1) <- withinEScope' (_bind2type . _defn2bind) ds1 (elimExpr e0)
    pure (ELet ds1 e1, t1)
  ERec ds0 e0 -> do
    withinEScope' (_bind2type . _defn2bind) ds0 $ do
      ds1 <- traverse elimDefn ds0
      (e1, t1) <- elimExpr e0
      pure (ERec ds1 e1, t1)
  EMat e0 (a0 :| as0) -> do
    (e1, _) <- elimExpr e0
    (a1, t1) <- elimAltn a0
    as1 <- traverse (fmap fst . elimAltn) as0
    pure (EMat e1 (a1 :| as1), t1)
  ETyAnn t_to (ETyCoe c e0) -> do
    (e1, _) <- elimExpr e0
    pure (ETyAnn t_to (ETyCoe c e1), t_to)
  ETyCoe{} ->
    bug "type coercion without surrounding type annotation during class elimination"
  ETyApp e0 ts0 -> do
    (e1, t_e1) <- elimExpr e0
    elimETyApp e1 t_e1 (toList ts0)
  ETyAbs qvs0 e0 -> elimETyAbs qvs0 e0
  ETyAnn t0 e0 -> do
    (e1, _) <- elimExpr e0
    pure (ETyAnn t0 e1, t0)

elimAltn ::
  (IsTVar tv, HasEnv ev) =>
  Altn In tv ev -> CE tv ev (Altn Out tv ev, Type tv)
elimAltn (MkAltn (PSimple dcon targs0 bnds) e0) = do
  (_, MkDConDecl _ _ _ flds0) <- findInfo info2dcons dcon
  let flds1 = map (instantiateN' targs0) flds0
  let env = Map.fromList
            (catMaybes (Safe.zipWithExact (\b t -> (,) <$> b <*> pure t) bnds flds1))
  withinEScope id env $ do
    (e1, t1) <- elimExpr e0
    pure (MkAltn (PSimple dcon targs0 bnds) e1, t1)

unclssType :: Type tv -> Type tv
unclssType = \case
  TVar v -> TVar v
  TAtm a -> TAtm a
  TApp tf tp -> TApp (unclssType tf) (unclssType tp)
  TUni qvs0 tq0 ->
    let qvs1 = fmap (qvar2cstr .~ mempty) qvs0
        dict_prms = do
          (i, MkQVar qual b) <- itoList qvs0
          let v = TVar (mkBound i b)
          clss <- toList qual
          pure (mkTDict clss v)
    in  TUni qvs1 (dict_prms *~> unclssType tq0)

unclssDecl :: Decl Out -> Decl Out
unclssDecl = \case
  DType tcons ->
    -- TODO: We're making the asusmption that type synonyms don't contain class
    -- constraints. This might change in the future.
    let tcon2type = tcon2dcons . _Right . traverse . dcon2flds . traverse
    in  DType (fmap (over tcon2type unclssType) tcons)
  DDefn defn -> run (runReader noPos (DDefn <$> defn2type (pure . unclssType) defn))
  DExtn extn -> DExtn (over (extn2bind . bind2type) unclssType extn)
