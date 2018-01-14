{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
module Pukeko.FrontEnd.TypeChecker
  ( checkModule
  ) where

import Pukeko.Prelude

import qualified Data.Map             as Map
import qualified Data.Vector.Sized    as Vec

import           Pukeko.Pretty
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.FrontEnd.Gamma
import           Pukeko.FrontEnd.Info
import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import qualified Pukeko.AST.ConDecl    as Con
import           Pukeko.AST.Type

checkModule :: (MonadError String m, St.Typed st) => Module st -> m (Module st)
checkModule module0@(MkModule tops) = do
  runTC (traverse_ checkTopLevel tops) module0
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
  EVal _ z -> fmap absurd . snd <$> findFun z
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
          throwErrorAt (ek^.pos) "expected type argument, but found value argument"
        _ -> throwErrorAt (ek^.pos) "unexpected value argument"
  ELam _ bs e0 t0 -> do
    let ts = fmap _bindType bs
    withTypes ts (check e0 t0)
    pure (ts *~> t0)
  ELet _ ds e0 -> do
    traverse_ checkDefn ds
    withBinds (fmap _defnLhs ds) (typeOf e0)
  ERec _ ds e0 -> do
    withBinds (fmap _defnLhs ds) $ do
      traverse_ checkDefn ds
      typeOf e0
  EMat _ e0 as -> typeOfBranching typeOfAltn e0 as
  ECas _ e0 cs -> typeOfBranching typeOfCase e0 cs
  ETyAbs _ xs e0 -> withKinds (TUni xs <$> typeOf e0)
  ETyApp w e0 ts1 -> do
    t0 <- typeOf e0
    case t0 of
      TUni xs t1 ->
        case Vec.matchList xs ts1 of
          Nothing -> throwDocAt w
            ("expected" <+> int (length xs) <+> "type arguments, but found"
             <+> int (length ts1) <+> "type arguments")
          Just ts2 ->
            pure (t1 >>= scope TVar (ts2 Vec.!))
      TFun{} ->
        throwErrorAt w "expected value argument, but found type argument"
      _ -> throwErrorAt w "unexpected type argument"

typeOfBranching ::
  (St.Typed st, IsTVar tv, IsEVar ev, HasPos branch) =>
  (Type tv -> branch -> TC tv ev (Type tv)) ->
  Expr st tv ev -> NonEmpty branch -> TC tv ev (Type tv)
typeOfBranching typeOfBranch e0 (b1 :| bs) = do
  t0 <- typeOf e0
  t1 <- typeOfBranch t0 b1
  for_ bs $ \b2 -> do
    t2 <- typeOfBranch t0 b2
    unless (t1 == t2) $
      throwDocAt (b2^.pos)
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
  (St.Typed st, IsTVar tv) =>
  Patn st tv -> Type tv -> TC tv ev (EnvLevelOf Id.EVar (Type tv))
patnEnvLevel p t0 = case p of
  PWld _   -> pure Map.empty
  PVar _ x -> pure (Map.singleton x t0)
  PCon w c ts1 ps -> do
    Some1 (Pair1 _tconDecl (Con.MkDConDecl tcon dcon _tag flds1)) <- findDCon c
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
  match (e^.pos) t0 t1

checkDefn :: (St.Typed st, IsEVar ev, IsTVar tv) => Defn st tv ev -> TC tv ev ()
checkDefn (MkDefn (MkBind _ _ t) e) = check e t

checkTopLevel :: (St.Typed st) => TopLevel st -> TC Void Void ()
checkTopLevel = \case
  TLTyp{} -> pure ()
  TLVal{} -> pure ()
  TLDef   d -> checkDefn d
  TLSup w z vs t0 bs e0 -> do
      t1 <- withKinds (withBinds bs (typeOf e0))
      let t2 = fmap _bindType bs *~> t1
      match w (mkTUni vs t0) (mkTUni vs t2)
    `catchError` \e -> throwError ("while type checking " ++ show z ++ ":\n" ++ e)
  TLAsm _ _ -> pure ()
