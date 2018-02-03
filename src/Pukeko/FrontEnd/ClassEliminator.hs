module Pukeko.FrontEnd.ClassEliminator
  ( elimModule
  ) where

import Pukeko.Prelude

import qualified Data.List.NE      as NE
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Vector       as Vec

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.ConDecl
import           Pukeko.AST.SystemF
import           Pukeko.AST.Stage
import           Pukeko.AST.Type
import           Pukeko.FrontEnd.Info
import           Pukeko.FrontEnd.Gamma

type In  = PatternMatcher
type Out = ClassEliminator

type IsTVar tv = (BaseTVar tv, HasEnv tv, Show tv)

type CE tv ev =
  EffXGamma (Map Id.Clss) Type tv ev [Reader ModuleInfo, Reader SourcePos]

runCE :: Module In -> CE Void Void a -> a
runCE m0 = run . runReader noPos . runInfo m0 . runGamma

elimModule :: Module In -> Module Out
elimModule m0@(MkModule decls) = runCE m0 $
  MkModule . map unclssDecl . concat <$> traverse elimDecl decls

elimDecl :: Decl In -> CE Void Void [Decl Out]
elimDecl decl = case decl of
  DClss clssDecl@(MkClssDecl (unlctd -> clss) prm mthds) -> do
    let tcon = dictTConDecl clssDecl
    let qprm = NE.singleton (MkQVar mempty prm)
        prmType = TVar (mkBound 0 prm)
        dictPrm = Id.evar "dict"
        sels = do
          (i, MkSignDecl (Lctd mpos z) t0) <- itoList mthds
          let t1 = TUni (NE.singleton (MkQVar (Set.singleton clss) prm)) t0
          let e_rhs = EVar (mkBound i z)
          let c_binds = imap (\j _ -> guard (i==j) *> pure z) mthds
          let c_one = MkCase (clssDCon clss) [prmType] c_binds e_rhs
          let e_cas = ECas (EVar (mkBound 0 dictPrm)) (c_one :| [])
          let b_lam = MkBind (Lctd mpos dictPrm) (mkTDict clss prmType)
          let e_lam = ELam (NE.singleton b_lam) e_cas t0
          let e_tyabs = ETyAbs qprm e_lam
          pure (DDefn (MkDefn (MkBind (Lctd mpos z) t1) e_tyabs))
    pure (DType (NE.singleton tcon) : sels)
  DInst inst@(MkInstDecl (Lctd ipos clss) tcon qvs defns0) -> do
    let t_inst = mkTApp (TCon tcon) (mkTVarsQ qvs)
    let (z_dict, t_dict) = instDictInfo inst
    clssDecl@(MkClssDecl _ _ mthds) <- findInfo info2clsss clss
    local (<> tconDeclInfo (dictTConDecl clssDecl)) $ do
        defns1 <- for mthds $ \(MkSignDecl (unlctd -> mthd) _) -> do
          case find (\defn -> defn^.defn2func == mthd) defns0 of
            Nothing -> bugWith "missing method" (clss, tcon, mthd)
            Just defn -> pure defn
        let e_dcon = mkETyApp (ECon (clssDCon clss)) [t_inst]
        let e_body =
              mkEApp e_dcon (imap (\i -> EVar . mkBound i . (^.defn2func)) defns1)
        let e_let :: Expr In _ _
            e_let = ELet defns1 e_body
        let e_rhs :: Expr In _ _
            e_rhs = mkETyAbs qvs e_let
        elimDecl (DDefn (MkDefn (MkBind (Lctd ipos z_dict) t_dict) e_rhs))
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
    pure (mkEApp (mkETyApp e0 ts0) dicts, instantiateN ts0 t_e1)

elimDefn :: (IsTVar tv, HasEnv ev) => Defn In tv ev -> CE tv ev (Defn Out tv ev)
elimDefn = defn2exprSt (fmap fst . elimExpr)

elimExpr ::
  (IsTVar tv, HasEnv ev) => Expr In tv ev -> CE tv ev (Expr Out tv ev, Type tv)
elimExpr = \case
  ELoc (Lctd pos e0) -> here_ pos $ first (ELoc . Lctd pos) <$> elimExpr e0
  EVar x -> (,) <$> pure (EVar x) <*> lookupEVar x
  EAtm a -> (,) <$> pure (EAtm a) <*> typeOfAtom a
  EApp e0 es0 -> do
    (e1, t1) <- elimExpr e0
    (es1, ts1) <- NE.unzip <$> traverse elimExpr es0
    pure (EApp e1 es1, unTFun t1 (toList ts1))
  ELam bs e0 t0 -> do
    let ts = fmap _bind2type bs
    (e1, t1) <- withinEScope' id ts (elimExpr e0)
    pure (ELam bs e1 t0, ts *~> t1)
  ELet ds0 e0 -> do
    ds1 <- traverse elimDefn ds0
    (e1, t1) <- withinEScope' (_bind2type . _defn2bind) ds1 (elimExpr e0)
    pure (ELet ds1 e1, t1)
  ERec ds0 e0 -> do
    withinEScope' (_bind2type . _defn2bind) ds0 $ do
      ds1 <- traverse elimDefn ds0
      (e1, t1) <- elimExpr e0
      pure (ERec ds1 e1, t1)
  ECas e0 (c0 :| cs0) -> do
    (e1, _) <- elimExpr e0
    (c1, t1) <- elimCase c0
    cs1 <- traverse (fmap fst . elimCase) cs0
    pure (ECas e1 (c1 :| cs1), t1)
  ECoe c e0 -> (,) <$> (ECoe c . fst <$> elimExpr e0) <*> pure (_coeTo c)
  ETyApp e0 ts0 -> do
    (e1, t_e1) <- elimExpr e0
    elimETyApp e1 t_e1 (toList ts0)
  ETyAbs qvs0 e0 -> do
    pos <- where_
    let ixbs = do
          (i, MkQVar qual v) <- itoList qvs0
          clss <- toList qual
          let x = dictEVar clss v

          pure ((i, clss, x), MkBind (Lctd pos x) (mkTDict clss (TVar (mkBound i v))))
    let (ixs, bs) = unzip ixbs
    let refs = Vec.accum Map.union (Vec.replicate (length qvs0) mempty)
               [ (i, Map.singleton clss (mkBound j x))
               | (j, (i, clss, x)) <- itoList ixs
               ]
    (e1, t1) <-
      withinXScope refs (Vec.fromList (fmap _bind2type bs)) (elimExpr (weakenE e0))
    let qvs1 = fmap (qvar2cstr .~ mempty) qvs0
    pure (ETyAbs qvs1 (mkELam bs e1 t1), TUni qvs0 t1)

elimCase ::
  (IsTVar tv, HasEnv ev) =>
  Case In tv ev -> CE tv ev (Case Out tv ev, Type tv)
elimCase (MkCase dcon targs0 bnds e0) = do
  (_, MkDConDecl _ _ _ flds0) <- findInfo info2dcons dcon
  let flds1 = map (instantiateN' targs0) flds0
  withinEScope' id flds1 $ do
    (e1, t1) <- elimExpr e0
    pure (MkCase dcon targs0 bnds e1, t1)

unTFun :: Type tv -> [Type tv] -> Type tv
unTFun = curry $ \case
  (t        , []  ) -> t
  (TFun _ ty, _:ts) -> unTFun ty ts
  _                 -> bug "too many arguments"

unclssType :: Type tv -> Type tv
unclssType = \case
  TVar v -> TVar v
  TArr -> TArr
  TCon c -> TCon c
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
    -- TODO: We're making the assmption that type synonyms don't contain class
    -- constraints. This might change in the future.
    let tcon2type = tcon2dcons . _Right . traverse . dcon2flds . traverse
    in  DType (fmap (over tcon2type unclssType) tcons)
  DDefn defn -> run (runReader noPos (DDefn <$> defn2type (pure . unclssType) defn))
  DExtn extn -> DExtn (over (extn2bind . bind2type) unclssType extn)

-- TODO: Make this less hacky.
clssTCon :: Id.Clss -> Id.TCon
clssTCon clss = Id.tcon ("Dict$" ++ Id.name clss)

clssDCon :: Id.Clss -> Id.DCon
clssDCon clss = Id.dcon ("Dict$" ++ Id.name clss)

mkTDict :: Id.Clss -> Type tv -> Type tv
mkTDict clss t = mkTApp (TCon (clssTCon clss)) [t]

dictEVar :: (Id.Named t) => Id.Clss -> t -> Id.EVar
dictEVar clss tcon = Id.evar ("dict$" ++ Id.name clss ++ "$" ++ Id.name tcon)

instDictInfo :: InstDecl st -> (Id.EVar, Type Void)
instDictInfo (MkInstDecl (unlctd -> clss) tcon qvs _) =
    let t_dict = mkTUni qvs (mkTDict clss (mkTApp (TCon tcon) (mkTVarsQ qvs)))
    in  (dictEVar clss tcon, t_dict)

dictTConDecl :: ClssDecl -> TConDecl
dictTConDecl (MkClssDecl (Lctd pos clss) prm mthds) =
    let flds = map _sign2type mthds
        dcon = MkDConDecl (clssTCon clss) (Lctd pos (clssDCon clss)) 0 flds
    in  MkTConDecl (Lctd pos (clssTCon clss)) [prm] (Right [dcon])
