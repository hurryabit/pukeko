{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
module Pukeko.FrontEnd.TypeChecker
  ( checkModule
  ) where

import Pukeko.Prelude

import qualified Data.Map             as Map
import qualified Data.Set             as Set
import qualified Data.Vector.Sized    as Vec

import           Pukeko.Pretty
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Type

checkModule :: (MonadError String m, St.Typed st) => Module st -> m (Module st)
checkModule module0@(MkModule tops) = do
  runTC (traverse_ checkDecl tops) module0
  pure module0

type IsEVar ev = (HasEnv ev)

type IsTVar tv = (Eq tv, HasEnv tv, BaseTVar tv)

type TC tv ev a = GammaT tv ev (InfoT (Except String)) a

runTC ::
  (MonadError String m, IsType (St.StageType st)) =>
  TC Void Void a -> Module st -> m a
runTC tc m0 = runExcept (runInfoT (runGammaT tc) m0)

typeOf :: (St.Typed st, IsEVar ev, IsTVar tv) => Expr st tv ev -> TC tv ev (Type tv)
typeOf = \case
  EVar _ x -> lookupType x
  EVal _ z -> fmap absurd . _sign2type <$> findInfo info2signs z
  ECon _ c -> fmap absurd <$> typeOfDCon c
  ENum _ _ -> pure typeInt
  EApp _ e0 es -> do
    t0 <- typeOf e0
    foldlM app t0 es
    where
      app tf ek = case tf of
        TFun tx ty -> do
          check ek tx
          pure ty
        TUni{} ->
          throwErrorAt (ek^.expr2pos) "expected type argument, but found value argument"
        _ -> throwErrorAt (ek^.expr2pos) "unexpected value argument"
  ELam _ bs e0 t0 -> do
    let ts = fmap _bind2type bs
    withTypes ts (check e0 t0)
    pure (ts *~> t0)
  ELet _ ds e0 -> do
    traverse_ checkDefn ds
    withBinds (fmap _defn2bind ds) (typeOf e0)
  ERec _ ds e0 -> do
    withBinds (fmap _defn2bind ds) $ do
      traverse_ checkDefn ds
      typeOf e0
  EMat _ e0 as -> typeOfBranching _altn2pos typeOfAltn e0 as
  ECas _ e0 cs -> typeOfBranching _case2pos typeOfCase e0 cs
  ETyAbs _ qvs e0 -> withQVars qvs (TUni qvs <$> typeOf e0)
  ETyApp w e0 ts1 -> do
    t0 <- typeOf e0
    case t0 of
      TUni qvs t1 ->
        case Vec.matchNonEmpty qvs ts1 of
          Nothing -> throwDocAt w
            ("expected" <+> int (length qvs) <+> "type arguments, but found"
             <+> int (length ts1) <+> "type arguments")
          Just ts2 -> do
            here w (Vec.zipWithM_ satisfiesCstrs ts2 qvs)
            pure (t1 >>= scope TVar (ts2 Vec.!))
      TFun{} ->
        throwErrorAt w "expected value argument, but found type argument"
      _ -> throwErrorAt w "unexpected type argument"

satisfiesCstrs :: (IsTVar tv) => Type tv -> QVar -> TC tv ev ()
satisfiesCstrs t (MkQVar q _) = traverse_ (satisfiesCstr t) q

satisfiesCstr :: (IsTVar tv) => Type tv -> Id.Clss -> TC tv ev ()
satisfiesCstr t0 clss = do
  let (t1, tps) = gatherTApp t0
  let throwNoInst = throwDoc ("no instance for" <+> pretty clss <+> parens (pretty t1))
  case t1 of
    TCon tcon -> do
      qvs_mb <- lookupInfo info2insts (clss, tcon)
      case qvs_mb of
        Nothing -> throwNoInst
        Just qvs -> do
          unless (length tps == length qvs) $
            -- NOTE: This should be caught by the kind checker.
            bugWith "mitmatching number of type arguments for instance" (clss, tcon)
          zipWithM_ satisfiesCstrs tps qvs
    TVar v
      | null tps -> do
          qual <- lookupKind v
          unless (clss `Set.member` qual) throwNoInst
    _ -> throwNoInst


typeOfBranching ::
  (St.Typed st, IsTVar tv, IsEVar ev) =>
  (branch -> Pos) ->
  (Type tv -> branch -> TC tv ev (Type tv)) ->
  Expr st tv ev -> NonEmpty branch -> TC tv ev (Type tv)
typeOfBranching pos typeOfBranch e0 (b1 :| bs) = do
  t0 <- typeOf e0
  t1 <- typeOfBranch t0 b1
  for_ bs $ \b2 -> do
    t2 <- typeOfBranch t0 b2
    unless (t1 == t2) $
      throwDocAt (pos b2)
        ("expected type" <+> pretty t1 <> ", but found type" <+> pretty t2)
  pure t1

typeOfAltn ::
  (St.Typed st, IsTVar tv, IsEVar ev) =>
  Type tv -> Altn st tv ev -> TC tv ev (Type tv)
typeOfAltn t (MkAltn _ p e) = do
  env <- patnEnvLevel p t
  withTypes env (typeOf e)

typeOfCase ::
  (St.Typed st, IsTVar tv, IsEVar ev) =>
  Type tv -> Case st tv ev -> TC tv ev (Type tv)
typeOfCase t (MkCase w c ts bs e0) = do
  let ps = map (PVar w) (toList bs)
      e1 = fmap (first (bs Vec.!)) e0
  typeOfAltn t (MkAltn w (PCon w c ts ps) e1)

patnEnvLevel ::
  (IsTVar tv) => Patn Type tv -> Type tv -> TC tv ev (EnvLevelOf Id.EVar (Type tv))
patnEnvLevel p t0 = case p of
  PWld _   -> pure Map.empty
  PVar _ x -> pure (Map.singleton x t0)
  PCon w c ts1 ps -> do
    Some1 (Pair1 _tconDecl (MkDConDecl _ tcon dcon _tag flds1)) <- findInfo info2dcons c
    let t1 = mkTApp (TCon tcon) (toList ts1)
    unless (t0 == t1) $ throwDocAt w
      ("expected pattern of type" <+> pretty t0
       <> ", but found pattern of type" <+> pretty t1)
    unless (length flds1 == length ps) $ throwDocAt w
      ("expected" <+> int (length flds1) <+> "pattern arguments of" <+> pretty dcon
       <> ", but found" <+> int (length ps) <+> "pattern arguments")
    Vec.withList ts1 $ \ts2 -> do
      let fldsProxy :: [Type (TFinScope n tv)] -> Proxy n
          fldsProxy _ = Proxy
      case sameNat (Vec.plength ts2) (fldsProxy flds1) of
        Nothing -> bugWith "mismatching kinds for type constructor" tcon
        Just Refl -> do
          let t_ps = map (>>= scope absurd (ts2 Vec.!)) flds1
          Map.unions <$> zipWithM patnEnvLevel ps t_ps

match :: (IsTVar tv) => Pos -> Type tv -> Type tv -> TC tv ev ()
match w t0 t1 =
  unless (t0 == t1) $
    throwDocAt w ("expected type" <+> pretty t0 <> ", but found type" <+> pretty t1)

check :: (St.Typed st, IsTVar tv, IsEVar ev) => Expr st tv ev -> Type tv -> TC tv ev ()
check e t0 = do
  t1 <- typeOf e
  match (e^.expr2pos) t0 t1

checkDefn :: (St.Typed st, IsEVar ev, IsTVar tv) => Defn st tv ev -> TC tv ev ()
checkDefn (MkDefn (MkBind _ _ t) e) = check e t

checkDecl :: (St.Typed st) => Decl st -> TC Void Void ()
checkDecl = \case
  DType{} -> pure ()
  DSign{} -> pure ()
  DClss{} -> pure ()
  -- FIXME: Check types in method definitions.
  DInst{} -> pure ()
  DDefn   d -> checkDefn d
  DSupC (MkSupCDecl w z qvs t0 bs e0) -> do
      t1 <- withQVars qvs (withBinds bs (typeOf e0))
      let t2 = fmap _bind2type bs *~> t1
      match w (mkTUni qvs t0) (mkTUni qvs t2)
    `catchError` \e -> throwError ("while type checking " ++ show z ++ ":\n" ++ e)
  DPrim _ -> pure ()
