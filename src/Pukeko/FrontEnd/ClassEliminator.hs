module Pukeko.FrontEnd.ClassEliminator
  ( elimModule
  ) where

import Pukeko.Prelude

-- import qualified Bound.Name as B
import qualified Bound.Scope as B
import qualified Safe

import           Pukeko.AST.ConDecl
import           Pukeko.AST.Dict
import           Pukeko.AST.Name
import           Pukeko.AST.SystemF
import           Pukeko.AST.Language
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Info

type In  = Unnested
type Out = Unclassy

type CanCE effs = Members [Reader SourcePos, Reader ModuleInfo, NameSource] effs
type CE a = forall effs. CanCE effs => Eff effs a

elimModule :: Member NameSource effs => Module In -> Eff effs (Module Out)
elimModule m0@(MkModule decls0) = runInfo m0 $ do
  decls1 <- for decls0 $ \decl0 -> elimDecl decl0 & runReader (getPos decl0)
  pure (MkModule (map unclssDecl (concat decls1)))

-- | Apply the dictionary type constructor of a type class to a type. For the
-- @List@ instance of @Traversable@, we obtain @Dict$Traversable List$, i.e.,
--
-- > TApp (TCon "Dict$Traversable") [TCon "List"]
mkTDict :: GenTypeCstr tv -> GenType tv
mkTDict (clss, t) = TApp (TCon clss) t

-- | Construct the dictionary data type declaration of a type class declaration.
-- See 'elimClssDecl' for an example.
dictTyConDecl :: ClassDecl -> TyConDecl
dictTyConDecl (MkClassDecl clss prm dcon mthds) =
    let flds = map _sign2type mthds
        dconDecl = MkTmConDecl clss dcon 0 flds
    in  MkTyConDecl clss [prm] (Right [dconDecl])

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
elimClssDecl :: ClassDecl -> CE [Decl Out]
elimClssDecl clssDecl@(MkClassDecl clss prm dcon mthds) = do
  let tcon = dictTyConDecl clssDecl
  let cstr = (clss, TVar prm)
  sels <- ifor mthds $ \i (MkSignDecl z t0) -> do
    dictPrm <- mkName (Lctd noPos "dict")
    let dictType = mkTDict cstr
    let t1 = TUni' prm (TCtx cstr t0)
    let e_rhs = EVar z
    let c_binds = imap (\j _ -> guard (i==j) $> (z, t0)) mthds
    let c_one = MkAltn (PSimple dcon c_binds) e_rhs
    let e_cas = EMat dictType (EVar dictPrm) (c_one :| [])
    let b_lam = (dictPrm, dictType)
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
elimInstDecl :: ClassDecl -> InstDecl In -> CE [Decl In]
elimInstDecl
  (MkClassDecl _ _ dcon methods0)
  (MkInstDecl inst clss tatom prms ctxt defns0) = do
  let t_inst = mkTApp (TAtm tatom) (map TVar prms)
  let t_dict0 = mkTDict (clss, t_inst)
  let mkFuncDecl name type0 body0 =
        -- TODO: We might want to copy the dictionary binders here.
        let type1 = rewindr TUni' prms (rewindr TCtx (map snd ctxt) type0)
            body1 = rewindr ETyAbs prms (rewindr ECxAbs ctxt (ETyAnn type0 body0))
        in  MkFuncDecl name type1 body1
  defns1 <- for methods0 $ \(MkSignDecl mthd _) -> do
    let (MkFuncDecl name0 typ_ body) =
          Safe.findJustNote "BUG" (\defn -> nameOf defn == mthd) defns0
    let name1 = Tagged (untag (nameText inst) ++ "." ++ untag (nameText name0))
    name2 <- mkName (Lctd (getPos name0) name1)
    pure (mkFuncDecl name2 typ_ body)
  let e_dcon = foldl ETyApp (ECon dcon) [t_inst]
  let callDefn defn =
        foldl ECxApp
          (foldl ETyApp (EVal (nameOf defn)) (map TVar prms))
          (map (DVar . fst) ctxt)
  let e_body = foldl ETmApp e_dcon (map callDefn defns1)
  pure (map DFunc (mkFuncDecl inst t_dict0 e_body : defns1))

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
    clss <- findInfo info2classes (inst^.inst2class)
    local (<> tyconDeclInfo (dictTyConDecl clss)) $ do
      defns <- elimInstDecl clss inst
      concat <$> traverse elimDecl defns

elimDict :: Dict -> Expr Out
elimDict = \case
  DVar x -> EVar x
  DDer z ts ds -> foldl ETmApp (foldl ETyApp (EVal z) ts) (map elimDict ds)

elimExpr :: Expr In -> CE (Expr Out)
elimExpr = \case
  ELoc (Lctd pos e0) -> here_ pos $ elimExpr e0
  EVar x -> pure (EVar x)
  EAtm a -> pure (EAtm a)
  ETmApp fun arg -> ETmApp <$> elimExpr fun <*> elimExpr arg
  ETyApp e0 ts0 -> ETyApp <$> elimExpr e0 <*> pure ts0
  ECxApp e0 dict -> ETmApp <$> elimExpr e0 <*> pure (elimDict dict)
  EApp{} -> impossible
  ETmAbs binder body0 -> ETmAbs binder <$> elimExpr body0
  ETyAbs vs0 e0 -> ETyAbs vs0 <$> elimExpr e0
  ECxAbs (x, c) e0 -> ETmAbs (x, mkTDict c) <$> elimExpr e0
  EAbs{} -> impossible
  ELet m ds0 e0 -> ELet m <$> traverse (b2bound elimExpr) ds0 <*> elimExpr e0
  EMat t e0 as -> EMat t <$> elimExpr e0 <*> traverse (altn2expr elimExpr) as
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
    let tcon2type = tycon2tmcons . _Right . traverse . tmcon2fields . traverse
    in  DType (over tcon2type unclssType tcon)
  DFunc (MkFuncDecl name typ0 body0) ->
    let body1 = over expr2type unclssType body0
        typ1 = unclssType typ0
    in  DFunc (MkFuncDecl name typ1 body1)
  DExtn extn -> DExtn (over extn2type unclssType extn)
