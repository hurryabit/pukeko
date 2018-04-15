module Pukeko.FrontEnd.ClassEliminator
  ( elimModule
  ) where

import Pukeko.Prelude

import qualified Bound.Scope as B
import           Data.Maybe (maybeToList)
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

type CanCE effs = Members [Reader ModuleInfo, NameSource] effs
type CE a = forall effs. CanCE effs => Eff effs a

elimModule :: Member NameSource effs => Module In -> Eff effs (Module Out)
elimModule m0@(MkModule decls0) = runInfo m0 $
  MkModule . map unclssDecl . concat <$> traverse elimDecl decls0

-- | Apply the dictionary type constructor of a type class to a type. For the
-- @List@ instance of @Traversable@, we obtain @Dict$Traversable List$, i.e.,
--
-- > TApp (TCon "Dict$Traversable") [TCon "List"]
mkTDict :: GenTypeCstr tv -> GenType tv
mkTDict (clss, t) = TApp (TCon clss) t

-- | Construct the dictionary data type declaration of a type class declaration.
-- See 'elimClssDecl' for an example.
dictTyConDecl :: ClassDecl -> TyConDecl
dictTyConDecl (MkClassDecl clss prm super dcon mthds) =
    let superFld = fmap (\(_, superClass) -> mkTDict (superClass, TVar prm)) super
        flds = maybeToList superFld  ++ map _sign2type mthds
        dconDecl = MkTmConDecl clss dcon 0 flds
    in  MkTyConDecl clss [prm] (Right [dconDecl])

mkProjExpr
  :: Member NameSource effs
  => Type      -- ^ Type of the scrutinee.
  -> Expr Out  -- ^ Scrutinee.
  -> TmCon     -- ^ Constructor of the record type.
  -> Int       -- ^ Arity of the constructor aka. number of fields.
  -> Int       -- ^ Index of the field to project on.
  -> TmVar     -- ^ Name of the field to project on.
  -> Type      -- ^ Type of the field to project on.
  -> Eff effs (Expr Out)
mkProjExpr t_scrut scrut tmcon arity index field t_field = do
  let patn = PSimple tmcon $
        replicate index Nothing
        ++ [Just (field, t_field)]
        ++ replicate (arity-index-1) Nothing
  pure (EMat t_scrut scrut (MkAltn patn (EVar field) :| []))

-- TODO: This documentation is out of date.
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
elimClssDecl clssDecl@(MkClassDecl clss prm super dcon mthds0) = do
  let tcon = dictTyConDecl clssDecl
  let cstr = (clss, TVar prm)
  let dictType = mkTDict cstr
  let superMthd = fmap (\(z, c) -> MkSignDecl z (mkTDict (c, TVar prm))) super
  let mthds1 = maybeToList superMthd ++ mthds0
  let numMthds = length mthds1
  sels <- ifor mthds1 $ \i (MkSignDecl z t_mthd) -> do
    dictPrm <- mkName (Lctd noPos "dict")
    projVar <- copyName z
    proj <- mkProjExpr dictType (EVar dictPrm) dcon numMthds i projVar t_mthd
    let body = ETyAbs prm $ ETmAbs (dictPrm, dictType) $ ETyAnn t_mthd proj
    let t_body = TUni' prm (TCtx cstr t_mthd)
    pure (DFunc (MkFuncDecl z t_body body))
  pure (DType tcon : sels)

-- TODO: This documentation is out of date.
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
elimInstDecl :: InstDecl In -> CE [Decl Out]
elimInstDecl (MkInstDecl inst clss tatom prms ctxt superI defns0) = do
  MkClassDecl _ _ superC dcon methods0 <- findInfo info2classes clss
  let t_inst = mkTApp (TAtm tatom) (map TVar prms)
  let t_dict0 = mkTDict (clss, t_inst)
  let mkFuncDecl name type0 body0 = do
        -- TODO: We might want to copy the dictionary binders here.
        let type1 = rewindr TUni' prms (rewindr TCtx (map snd ctxt) type0)
        let body1 = rewindr ETyAbs prms (rewindr ECxAbs ctxt (ETyAnn type0 body0))
        MkFuncDecl name type1 <$> elimExpr body1
  -- TODO: Share code between @superDefn@ und @defns1@.
  superDefn <- for superI $ \dict -> do
    let (_, super) = Safe.fromJustNote "BUG" superC
    name1 <- synthName (getPos inst) inst super
    mkFuncDecl name1 (mkTDict (super, t_inst)) (elimDict dict)
  defns1 <- for methods0 $ \(MkSignDecl mthd _) -> do
    let MkFuncDecl name0 typ_ body =
          Safe.findJustNote "BUG" (\defn -> nameOf defn == mthd) defns0
    name1 <- synthName (getPos name0) inst name0
    mkFuncDecl name1 typ_ body
  let defns2 = maybeToList superDefn ++ defns1
  let e_dcon = foldl ETyApp (ECon dcon) [t_inst]
  let callDefn defn =
        foldl ECxApp
          (foldl ETyApp (EVal (nameOf defn)) (map TVar prms))
          (map (DVar . fst) ctxt)
  let e_body = foldl ETmApp e_dcon (map callDefn defns2)
  dictDefn <- mkFuncDecl inst t_dict0 e_body
  pure (map DFunc (dictDefn : defns2))

elimDecl :: Decl In -> CE [Decl Out]
elimDecl = \case
  DType tcons -> pure [DType tcons]
  DFunc (MkFuncDecl name typ_ body) ->
    (:[]) . DFunc . MkFuncDecl name typ_ <$> elimExpr body
  DExtn (MkExtnDecl name typ_ extn) -> pure [DExtn (MkExtnDecl name typ_ extn)]
  DClss clss  -> elimClssDecl clss
  DInst inst  -> elimInstDecl inst

elimDict :: (IsTyped lg, HasTyApp lg) => Dict -> Expr lg
elimDict = \case
  DVar x -> EVar x
  DDer z ts ds -> foldl ETmApp (foldl ETyApp (EVal z) ts) (map elimDict ds)
  DSub z _c t d -> ETmApp (ETyApp (EVal z) t) (elimDict d)

elimExpr :: Expr In -> CE (Expr Out)
elimExpr = \case
  ELoc (Lctd _pos e0) -> elimExpr e0
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
