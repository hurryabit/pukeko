{-# LANGUAGE ConstraintKinds #-}
module Pukeko.FrontEnd.ClassEliminator
  ( elimModule
  ) where

import Pukeko.Prelude

import qualified Data.Finite       as Fin
import qualified Data.List.NonEmpty as NE
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Vector.Sized as Vec

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

type CE tv ev = XGammaT (Map Id.Clss) Type tv ev (InfoT (HereT Identity))

runCE :: Module In -> CE Void Void a -> a
runCE m0 ce = runIdentity (runHereT (runInfoT (runGammaT ce) m0))

elimModule :: Module In -> Module Out
elimModule m0@(MkModule decls) = runCE m0 $
  MkModule . fmap (fmap unclssDecl) . concatMap sequence
  <$> traverseHeres elimDecl decls

elimDecl :: Decl In -> CE Void Void [Decl Out]
elimDecl decl = case decl of
  DClss clssDecl@(MkClssDecl clss prm mthdsL) -> Vec.withList mthdsL $ \mthdsV -> do
    pos <- where_
    let tcon = dictTConDecl (Loc pos clssDecl)
    let qprm = Vec.singleton (MkQVar mempty prm)
        prmType = TVar (mkBound Fin.zero prm)
        dictPrm = Id.evar "dict"
        c_binds = fmap _sign2func mthdsV
        sels = do
          (i, MkSignDecl z t0) <- itoList mthdsV
          let t1 = TUni (Vec.singleton (MkQVar (Set.singleton clss) prm)) t0
          let e_rhs = EVar (mkBound i z)
          let c_one = MkCase (clssDCon clss) [prmType] c_binds e_rhs
          let e_cas = ECas (EVar (mkBound Fin.zero dictPrm)) (c_one :| [])
          let b_lam = MkBind dictPrm (mkTDict clss prmType)
          let e_lam = ELam (Vec.singleton b_lam) e_cas t0
          let e_tyabs = ETyAbs qprm e_lam
          pure (DDefn (MkDefn (MkBind z t1) e_tyabs))
    pure (DType (Loc pos tcon :| []) : sels)
  DInst inst@(MkInstDecl clss tcon qvs defns0) -> do
    let t_inst = mkTApp (TCon tcon) (mkTVarsQ qvs)
    let (z_dict, t_dict) = instDictInfo inst
    clssDecl@(MkClssDecl _ _ mthdsL) <- findInfo info2clsss clss
    pos <- where_
    mapGammaT (localInfo (tconDeclInfo (dictTConDecl (Loc pos clssDecl)))) $ do
      Vec.withList mthdsL $ \mthdsV -> do
        defns1 <- for mthdsV $ \(MkSignDecl mthd _) -> do
          case find (\defn -> unloc defn^.defn2func == mthd) defns0 of
            Nothing -> bugWith "missing method" (clss, tcon, mthd)
            Just defn -> pure defn
        let e_dcon = mkETyApp (ECon (clssDCon clss)) [t_inst]
        let e_body =
              mkEApp e_dcon
                (imap (\i -> EVar . mkBound i . view defn2func . unloc) defns1)
        let e_let :: Expr In _ _
            e_let = ELet defns1 e_body
        let e_rhs :: Expr In _ _
            e_rhs = mkETyAbs qvs e_let
        elimDecl (DDefn (MkDefn (MkBind z_dict t_dict) e_rhs))
  DDefn defn  -> (:[]) . DDefn <$> elimDefn defn
  DType tcons -> pure [DType tcons]
  DPrim prim  -> pure [DPrim prim]

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
  withTUni t_e0 $ \qvs t_e1 -> do
    let ts1 = Vec.matchList' qvs ts0
    let dictBldrs = do
          (MkQVar qual _, t1) <- toList (Vec.zip qvs ts1)
          clss <- toList qual
          pure (buildDict clss t1)
    dicts <- sequence dictBldrs
    pure (mkEApp (mkETyApp e0 ts0) dicts, t_e1 >>= scope TVar (ts1 Vec.!))

elimDefn :: (IsTVar tv, HasEnv ev) => Defn In tv ev -> CE tv ev (Defn Out tv ev)
elimDefn = defn2exprSt (fmap fst . elimExpr)

elimExpr ::
  (IsTVar tv, HasEnv ev) => Expr In tv ev -> CE tv ev (Expr Out tv ev, Type tv)
elimExpr = \case
  ELoc (Loc pos e0) -> here pos $ first (ELoc . Loc pos) <$> elimExpr e0

  EVar x -> (,) <$> pure (EVar x) <*> lookupEVar x
  EVal z -> (,) <$> pure (EVal z) <*> typeOfFunc z
  ECon c -> (,) <$> pure (ECon c) <*> typeOfDCon c
  ENum n -> pure (ENum n, typeInt)
  EApp e0 es0 -> do
    (e1, t1) <- elimExpr e0
    (es1, ts1) <- NE.unzip <$> traverse elimExpr es0
    pure (EApp e1 es1, unTFun t1 (toList ts1))
  ELam bs e0 t0 -> do
    let ts = fmap _bind2type bs
    (e1, t1) <- withinEScope ts (elimExpr e0)
    pure (ELam bs e1 t0, ts *~> t1)
  ELet ds0 e0 -> do
    ds1 <- traverseHeres elimDefn ds0
    (e1, t1) <- withinEScope (fmap (_bind2type . _defn2bind . unloc) ds1) (elimExpr e0)
    pure (ELet ds1 e1, t1)
  ERec ds0 e0 -> do
    withinEScope (fmap (_bind2type . _defn2bind . unloc) ds0) $ do
      ds1 <- traverseHeres elimDefn ds0
      (e1, t1) <- elimExpr e0
      pure (ERec ds1 e1, t1)
  ECas e0 (c0 :| cs0) -> do
    (e1, _) <- elimExpr e0
    (c1, t1) <- elimCase c0
    cs1 <- traverse (fmap fst . elimCase) cs0
    pure (ECas e1 (c1 :| cs1), t1)
  ETyApp e0 ts0 -> do
    (e1, t_e1) <- elimExpr e0
    elimETyApp e1 t_e1 (toList ts0)
  ETyAbs (qvs0 :: Vector m _) e0 -> do
    let ixbsL = do
          (i, MkQVar qual v) <- itoList qvs0
          clss <- toList qual
          let x = dictEVar clss v
          pure ((i, clss, x), MkBind x (mkTDict clss (TVar (mkBound i v))))
    Vec.withList ixbsL $ \(ixbsV :: Vector n _) -> do
      let (ixsV, bsV) = Vec.unzip ixbsV
      let refsL :: [(Finite m, Map Id.Clss (EFinScope n ev))]
          refsL = [ (i, Map.singleton clss (mkBound j x))
                 | (j, (i, clss, x)) <- itoList ixsV
                 ]
      let refsV = Vec.accum Map.union (fmap (const mempty) qvs0) refsL
      (e1, t1) <- withinXScope refsV (fmap _bind2type bsV) (elimExpr (fmap weaken e0))
      let qvs1 = fmap (qvar2cstr .~ mempty) qvs0
      pure (ETyAbs qvs1 (mkELam bsV e1 t1), mkTUni qvs0 t1)

elimCase ::
  (IsTVar tv, HasEnv ev) =>
  Case In tv ev -> CE tv ev (Case Out tv ev, Type tv)
elimCase (MkCase dcon targs0 bnds e0) = do
  Some1 (Pair1 _ (MkDConDecl _ dcon _ flds0)) <- findInfo info2dcons dcon
  let targs1 = Vec.fromList' targs0
  let flds1 = map (\fld -> fmap (fmap absurd) fld >>= scope TVar (targs1 Vec.!)) flds0
  let flds2 = Vec.matchList' bnds flds1
  withinEScope flds2 $ do
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
    let tcon2type = tcon2dcons . traverse . traverse . dcon2flds . traverse
    in  DType (fmapHeres (\(Some1 tcon) -> Some1 (over tcon2type unclssType tcon)) tcons)
  DDefn defn -> runIdentity (runHereT (DDefn <$> defn2type (pure . unclssType) defn))
  DPrim prim -> DPrim (over (prim2bind . bind2type) unclssType prim)

-- TODO: Make this less hacky.
clssTCon :: Id.Clss -> Id.TCon
clssTCon clss = Id.tcon ("Dict$" ++ show clss)

clssDCon :: Id.Clss -> Id.DCon
clssDCon clss = Id.dcon ("Dict$" ++ show clss)

mkTDict :: Id.Clss -> Type tv -> Type tv
mkTDict clss t = mkTApp (TCon (clssTCon clss)) [t]

dictEVar :: (Show tcon) => Id.Clss -> tcon -> Id.EVar
dictEVar clss tcon = Id.evar ("dict$" ++ show clss ++ "$" ++ show tcon)

instDictInfo :: InstDecl st -> (Id.EVar, Type Void)
instDictInfo (MkInstDecl clss tcon qvs _) =
    let t_dict = mkTUni qvs (mkTDict clss (mkTApp (TCon tcon) (mkTVarsQ qvs)))
    in  (dictEVar clss tcon, t_dict)

dictTConDecl :: Loc ClssDecl -> Some1 TConDecl
dictTConDecl (Loc pos (MkClssDecl clss prm mthds)) =
    let flds = map _sign2type mthds
        dcon = MkDConDecl (clssTCon clss) (clssDCon clss) 0 flds
        tcon = MkTConDecl (clssTCon clss) (Vec.singleton prm) [Loc pos dcon]
    in  Some1 tcon
