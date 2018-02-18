module Pukeko.FrontEnd.ClassEliminator
  ( elimModule
  ) where

import Pukeko.Prelude

import           Data.Coerce       (coerce)
import qualified Data.List.NE      as NE
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Vector       as Vec
import qualified Safe.Exact        as Safe

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Name
import           Pukeko.AST.SystemF
import           Pukeko.AST.Language
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Info
import           Pukeko.FrontEnd.Gamma

type In  = Unnested
type Out = Unclassy

type IsTVar tv = (BaseTVar tv, HasEnv tv, Show tv)

type CE tv ev =
  EffXGamma (Map (Name Clss)) Type tv ev [Reader ModuleInfo, Reader SourcePos, NameSource]

runCE :: Member NameSource effs => Module In -> CE Void Void a -> Eff effs a
runCE m0 = delayNameSource . runReader noPos . runInfo m0 . runGamma

elimModule :: Member NameSource effs => Module In -> Eff effs (Module Out)
elimModule m0@(MkModule decls) = runCE m0 $
  MkModule . map unclssDecl . concat <$> traverse elimDecl decls

-- | Name of the dictionary type constructor of a type class, e.g., @Dict$Eq@
-- for type class @Eq@.
clssTCon :: Name Clss -> Name TCon
clssTCon = coerce

-- | Apply the dictionary type constructor of a type class to a type. For the
-- @List@ instance of @Traversable@, we obtain @Dict$Traversable List$, i.e.,
--
-- > TApp (TCon "Dict$Traversable") [TCon "List"]
mkTDict :: Name Clss -> Type tv -> Type tv
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
instDictInfo :: InstDecl st -> (Name EVar, Type Void)
instDictInfo (MkInstDecl inst clss tatom qvs _) =
    let t_dict = mkTUni qvs (mkTDict clss (mkTApp (TAtm tatom) (mkTVarsQ qvs)))
    in  (inst, t_dict)

-- | Construct the dictionary data type declaration of a type class declaration.
-- See 'elimClssDecl' for an example.
dictTConDecl :: ClssDecl -> TConDecl
dictTConDecl (MkClssDecl clss prm dcon mthds) =
    let tcon = coerce clss
        flds = map _sign2type mthds
        dconDecl = MkDConDecl tcon dcon 0 flds
    in  MkTConDecl tcon [prm] (Right [dconDecl])

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
elimClssDecl clssDecl@(MkClssDecl clss prm dcon mthds) = do
  let tcon = dictTConDecl clssDecl
  let qprm = NE.singleton (MkQVar mempty prm)
  let prmType = TVar (mkBound 0 prm)
  dictPrm <- mkName (Lctd noPos "dict")
  let sels = do
        (i, MkSignDecl z t0) <- itoList mthds
        let t1 = TUni (NE.singleton (MkQVar (Set.singleton clss) prm)) t0
        let e_rhs = EVar (mkBound z z)
        let c_binds = imap (\j _ -> guard (i==j) *> pure z) mthds
        let c_one = MkAltn (PSimple dcon [prmType] c_binds) e_rhs
        let e_cas = EMat (EVar (mkBound () dictPrm)) (c_one :| [])
        let b_lam = MkBind dictPrm (mkTDict clss prmType)
        let e_lam = ELam b_lam (ETyAnn t0 e_cas)
        let e_tyabs = ETyAbs qprm e_lam
        pure (DFunc (MkFuncDecl z t1 e_tyabs))
  pure (DType tcon : sels)

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
elimInstDecl (MkClssDecl _ _ dcon methods0) inst@(MkInstDecl _ clss tatom qvs defns0) = do
  let t_inst = mkTApp (TAtm tatom) (mkTVarsQ qvs)
  let (z_dict, t_dict) = instDictInfo inst
  defns1 <- for methods0 $ \(MkSignDecl mthd _) -> do
    case find (\defn -> nameOf defn == mthd) defns0 of
      Nothing -> bugWith "missing method" (clss, tatom, mthd)
      Just (MkFuncDecl name0 typ_ body) -> do
        name1 <- copyName (getPos inst) name0
        pure (MkDefn (MkBind name1 typ_) body)
  let e_dcon = mkETyApp (ECon dcon) [t_inst]
  let e_body = foldl EApp e_dcon (imap (\i -> EVar . mkBound i . nameOf) defns1)
  let e_let :: Expr In _ _
      e_let = ELet defns1 e_body
  let e_rhs :: Expr In _ _
      e_rhs = mkETyAbs qvs e_let
  pure [DFunc (MkFuncDecl z_dict t_dict e_rhs)]

elimDecl :: Decl In -> CE Void Void [Decl Out]
elimDecl = here' $ \case
  DType tcons -> pure [DType tcons]
  DFunc (MkFuncDecl name typ_ body) ->
    (:[]) . DFunc . MkFuncDecl name typ_ . fst <$> elimExpr body
  DExtn (MkExtnDecl name typ_ extn) -> pure [DExtn (MkExtnDecl name typ_ extn)]
  DClss clss  -> elimClssDecl clss
  DInst inst  -> do
    -- TODO: Constructing the declaration of the dictionary type again and
    -- putting it in the environment locally is a bit a of a hack.
    clss <- findInfo info2clsss (inst^.inst2clss)
    local (<> tconDeclInfo (dictTConDecl clss)) $ do
      defns <- elimInstDecl clss inst
      concat <$> traverse elimDecl defns

buildDict :: (IsTVar tv) => Name Clss -> Type tv -> CE tv ev (Expr Out tv ev)
buildDict clss t0 = do
  let (t1, tps) = gatherTApp t0
  case t1 of
    TVar v
      | null tps ->
          lookupTVar v >>= maybe bugNoDict (pure . EVar) . Map.lookup clss
    TAtm tatom -> do
      SomeInstDecl inst <- findInfo info2insts (clss, tatom)
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

-- | Name of the dictionary for a type class instance of either a known type
-- ('Id.TCon') or an unknown type ('Id.TVar'), e.g., @dict@Traversable$List@ or
-- @dict$Monoid$m@.
dictEVar :: Name Clss -> Id.TVar -> CE tv ev (Name EVar)
dictEVar clss tcon =
  mkName (Lctd noPos (Tagged ("dict$" ++ untag (nameText clss) ++ "$" ++ Id.name tcon)))

-- | Enrich a type abstraction with value abstractions for type class
-- dictionaries.
elimETyAbs :: (IsTVar tv, HasEnv ev) =>
  NonEmpty QVar -> Expr In (TScope Int tv) ev -> CE tv ev (Expr Out tv ev, Type tv)
elimETyAbs qvs0 e0 = do
    -- TODO: This is horribly imperative.
    ixbs <- fmap concat . for (itoList qvs0) $ \(i, MkQVar qual v) ->
      for (toList qual) $ \clss -> do
          x <- dictEVar clss v
          pure ((i, clss, x), MkBind x (mkTDict clss (TVar (mkBound i v))))
    let (ixs, bs) = unzip ixbs
    let refs = Vec.accum Map.union (Vec.replicate (length qvs0) mempty)
               [ (i, Map.singleton (clss) (mkBound j x))
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
  DType tcon ->
    -- TODO: We're making the asusmption that type synonyms don't contain class
    -- constraints. This might change in the future.
    let tcon2type = tcon2dcons . _Right . traverse . dcon2fields . traverse
    in  DType (over tcon2type unclssType tcon)
  DFunc (MkFuncDecl name typ_ body0) ->
    let body1 = runIdentity (expr2type (Identity . unclssType) body0)
    in  DFunc (MkFuncDecl name (unclssType typ_) body1)
  DExtn extn -> DExtn (over extn2type unclssType extn)
