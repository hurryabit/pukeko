module Pukeko.FrontEnd.ClassEliminator
  ( elimModule
  ) where

import Pukeko.Prelude

-- import qualified Bound.Name as B
import qualified Bound.Scope as B
import qualified Data.Map.Extended as Map
import qualified Safe

import           Pukeko.AST.ConDecl
import           Pukeko.AST.Name
import           Pukeko.AST.SystemF
import           Pukeko.AST.Language
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Info

type In  = Unnested
type Out = Unclassy

type DictMap = Map (NameTVar, NameClss) NameEVar
type CanCE effs =
  Members [Reader DictMap, Reader SourcePos, Reader ModuleInfo, NameSource] effs
type CE a = forall effs. CanCE effs => Eff effs a

elimModule :: Member NameSource effs => Module In -> Eff effs (Module Out)
elimModule m0@(MkModule decls0) = runInfo m0 $ do
  decls1 <- for decls0 $ \decl0 -> elimDecl decl0
                                   & runReader @DictMap Map.empty
                                   & runReader (getPos decl0)
  pure (MkModule (map unclssDecl (concat decls1)))

-- | Apply the dictionary type constructor of a type class to a type. For the
-- @List@ instance of @Traversable@, we obtain @Dict$Traversable List$, i.e.,
--
-- > TApp (TCon "Dict$Traversable") [TCon "List"]
mkTDict :: GenTypeCstr tv -> GenType tv
mkTDict (clss, t) = TApp (TCon clss) t

-- | Construct the dictionary data type declaration of a type class declaration.
-- See 'elimClssDecl' for an example.
dictTConDecl :: ClssDecl -> TConDecl
dictTConDecl (MkClssDecl clss prm dcon mthds) =
    let flds = map _sign2type mthds
        dconDecl = MkDConDecl clss dcon 0 flds
    in  MkTConDecl clss [prm] (Right [dconDecl])

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
elimClssDecl :: ClssDecl -> CE [Decl Out]
elimClssDecl clssDecl@(MkClssDecl clss prm dcon mthds) = do
  let tcon = dictTConDecl clssDecl
  let cstr = (clss, TVar prm)
  sels <- ifor mthds $ \i (MkSignDecl z t0) -> do
    dictPrm <- mkName (Lctd noPos "dict")
    let t1 = TUni' prm (TCtx cstr t0)
    let e_rhs = EVar z
    let c_binds = imap (\j _ -> guard (i==j) $> z) mthds
    let c_one = MkAltn (PSimple dcon [TVar prm] c_binds) e_rhs
    let e_cas = EMat (EVar dictPrm) (c_one :| [])
    let b_lam = (dictPrm, mkTDict cstr)
    let e_lam = ETmAbs b_lam (ETyAnn t0 e_cas)
    let e_tyabs = ETyAbs prm e_lam
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
elimInstDecl :: ClssDecl -> InstDecl In -> CE [Decl In]
elimInstDecl
  (MkClssDecl _ _ dcon methods0)
  inst@(MkInstDecl z_dict clss tatom prms cstrs defns0) = do
  let t_inst = mkTApp (TAtm tatom) (map TVar prms)
  let t_dict0 = mkTDict (clss, t_inst)
  let t_dict = rewindr TUni' prms (rewindr TCtx cstrs t_dict0)
  defns1 <- for methods0 $ \(MkSignDecl mthd _) -> do
    let (MkFuncDecl name0 typ_ body) =
          Safe.findJustNote "BUG" (\defn -> nameOf defn == mthd) defns0
    name1 <- copyName (getPos inst) name0
    pure (MkBind (name1, typ_) body)
  let e_dcon = foldl ETyApp (ECon dcon) [t_inst]
  let e_body = foldl ETmApp e_dcon (map (EVar . nameOf) defns1)
  let e_let :: Expr In
      e_let = ELet defns1 e_body
  let e_rhs :: Expr In
      e_rhs = rewindr ETyAbs prms (rewindr ECxAbs cstrs (ETyAnn t_dict0 e_let))
  pure [DFunc (MkFuncDecl z_dict t_dict e_rhs)]

elimDecl :: Decl In -> CE [Decl Out]
elimDecl = here' $ \case
  DType tcons -> pure [DType tcons]
  DFunc (MkFuncDecl name typ_ body) ->
    (:[]) . DFunc . MkFuncDecl name typ_ <$> elimExpr body
  DExtn (MkExtnDecl name typ_ extn) -> pure [DExtn (MkExtnDecl name typ_ extn)]
  DClss clss  -> elimClssDecl clss
  DInst inst  -> do
    -- TODO: Constructing the declaration of the dictionary type again and
    -- putting it in the environment locally is a bit a of a hack.
    clss <- findInfo info2clsss (inst^.inst2clss)
    local (<> tconDeclInfo (dictTConDecl clss)) $ do
      defns <- elimInstDecl clss inst
      concat <$> traverse elimDecl defns

buildDict :: TypeCstr -> CE (Expr In)
buildDict (clss, unwindl _TApp -> (t1, args)) =
  case t1 of
    TVar v
      | null args -> EVar <$> asks @DictMap (Map.! (v, clss))
    TAtm tatom -> do
      SomeInstDecl (MkInstDecl inst _ _ pars cstrs _) <-
        findInfo info2insts (clss, tatom)
      let instArgs t = t >>= (Map.fromList (zipExact pars args) Map.!)
      pure (foldl ECxApp (foldl ETyApp (EVal inst) args) (map (second instArgs) cstrs))
    _ -> impossible  -- type checker would catch this

-- | Name of the dictionary for a type class instance of either a known type
-- ('Id.TCon') or an unknown type ('Id.TVar'), e.g., @dict@Traversable$List@ or
-- @dict$Monoid$m@.
dictEVar :: NameClss -> Name TVar -> CE (Name EVar)
dictEVar clss tvar = do
  let name0 = "dict$" ++ untag (nameText clss) ++ "$" ++ untag (nameText tvar)
  mkName (Lctd noPos (Tagged name0))

-- | Replace a context abstraction by a lambda for the dictionary.
elimECxAbs :: TypeCstr -> Expr In -> CE (Expr Out)
elimECxAbs cstr@(clss, TVar v) e0 = do
  x <- dictEVar clss v
  let dictBinder = (x, mkTDict cstr)
  ETmAbs dictBinder <$> local (Map.insertWith impossible (v, clss) x) (elimExpr e0)
elimECxAbs _ _ = impossible

elimECxApp :: Expr In -> TypeCstr -> CE (Expr Out)
elimECxApp e0 cstr = do
  e1 <- elimExpr e0
  dict0 <- buildDict cstr
  dict1 <- elimExpr dict0
  pure (ETmApp e1 dict1)

elimExpr :: Expr In -> CE (Expr Out)
elimExpr = \case
  ELoc (Lctd pos e0) -> here_ pos $ elimExpr e0
  EVar x -> pure (EVar x)
  EAtm a -> pure (EAtm a)
  ETmApp fun arg -> ETmApp <$> elimExpr fun <*> elimExpr arg
  ETyApp e0 ts0 -> ETyApp <$> elimExpr e0 <*> pure ts0
  ECxApp e0 cstr -> elimECxApp e0 cstr
  EApp{} -> impossible
  ETmAbs binder body0 -> ETmAbs binder <$> elimExpr body0
  ETyAbs vs0 e0 -> ETyAbs vs0 <$> elimExpr e0
  ECxAbs cstr e0 -> elimECxAbs cstr e0
  EAbs{} -> impossible
  ELet ds0 e0 -> ELet <$> traverse (b2bound elimExpr) ds0 <*> elimExpr e0
  ERec ds0 e0 -> ERec <$> traverse (b2bound elimExpr) ds0 <*> elimExpr e0
  EMat e0 as -> EMat <$> elimExpr e0 <*> traverse (altn2expr elimExpr) as
  ECast coe e0 -> ECast coe <$> elimExpr e0
  ETyAnn t0 e0 -> ETyAnn t0 <$> elimExpr e0

unclssType :: GenType tv -> GenType tv
unclssType = \case
  TVar v -> TVar v
  TAtm a -> TAtm a
  TApp tf tp -> TApp (unclssType tf) (unclssType tp)
  TUni vs tq -> TUni vs (B.hoistScope unclssType tq)
  TCtx cstr t -> mkTDict cstr ~> unclssType t

unclssDecl :: Decl Out -> Decl Out
unclssDecl = \case
  DType tcon ->
    -- TODO: We're making the asusmption that type synonyms don't contain class
    -- constraints. This might change in the future.
    let tcon2type = tcon2dcons . _Right . traverse . dcon2fields . traverse
    in  DType (over tcon2type unclssType tcon)
  DFunc (MkFuncDecl name typ0 body0) ->
    let body1 = over expr2type unclssType body0
        typ1 = unclssType typ0
    in  DFunc (MkFuncDecl name typ1 body1)
  DExtn extn -> DExtn (over extn2type unclssType extn)
