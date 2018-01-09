{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
module Pukeko.Language.TypeChecker
  ( checkModule
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Bifunctor       (first)
import           Data.Foldable
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map             as Map
import           Data.Proxy           (Proxy (..))
import           Data.Type.Equality   ((:~:) (..))
import qualified Data.Vector.Sized    as Vec
import           GHC.TypeLits

import           Pukeko.Pretty
import           Pukeko.Error
import           Pukeko.Pos
import qualified Pukeko.Language.Ident     as Id
import           Pukeko.Language.Info
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage as St
import qualified Pukeko.Language.AST.ConDecl as Con
import           Pukeko.Language.Type

-- FIXME: Drop @HasELam@ constraint.
type Typed st =
  ( St.StageType st ~ Type
  , St.HasTLTyp st  ~ 'False
  , St.HasTLVal st  ~ 'False
  , St.HasLambda  st  ~ 'True
  , St.HasMICons st ~ 'True
  , St.HasMIFuns st ~ 'True
  )

checkModule :: (MonadError String m, Typed st) => Module st -> m (Module st)
checkModule module0@(MkModule info tops) = do
  runTC (traverse_ checkTopLevel tops) info
  pure module0

type IsEVar ev = (HasEnv ev)

type IsTVar tv = (Eq tv, HasEnv tv, BaseTVar tv)

type ModuleInfoTC = GenModuleInfo 'True 'True

newtype TC tv ev a =
  TC{unTC :: InfoT ModuleInfoTC (ReaderT (EnvOf ev (Type tv)) (Except String)) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadInfo ModuleInfoTC
           )

runTC :: MonadError String m => TC Void Void a -> ModuleInfoTC -> m a
runTC tc mi =
  let env = Const ()
  in  runExcept (runReaderT (runInfoT (unTC tc) mi) env)

lookupType :: (IsEVar ev) => ev -> TC tv ev (Type tv)
lookupType = TC . asks . lookupEnv

localizeTypes ::
  forall i tv ev a. (HasEnvLevel i, IsEVar ev) =>
  EnvLevelOf i (Type tv) -> TC tv (EScope i ev) a -> TC tv ev a
localizeTypes bs =
  let f = extendEnv @i @ev bs
  in  TC . mapInfoT (withReaderT f) . unTC

localizeKinds :: (IsEVar ev) => TC (TFinScope i tv) ev a -> TC tv ev a
localizeKinds =
  let f = fmap (fmap Free)
  in  TC . mapInfoT (withReaderT f) . unTC

typeOf :: (Typed st, IsEVar ev, IsTVar tv) => Expr st tv ev -> TC tv ev (Type tv)
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
  ELam _ bs e0 -> do
    let ts = fmap _bindType bs
    t0 <- localizeTypes ts (typeOf e0)
    pure (toList ts *~> t0)
  ELet _ ds e0 -> do
    traverse_ checkDefn ds
    -- TODO: Make lenses for this combination.
    localizeTypes (fmap (_bindType . _defnLhs) ds) (typeOf e0)
  ERec _ ds e0 -> do
    localizeTypes (fmap (_bindType . _defnLhs) ds) $ do
      traverse_ checkDefn ds
      typeOf e0
  EMat _ e0 as -> typeOfBranching typeOfAltn e0 as
  ECas _ e0 cs -> typeOfBranching typeOfCase e0 cs
  ETyAbs _ xs e0 -> localizeKinds (TUni xs <$> typeOf e0)
  ETyApp w e0 ts1 -> do
    t0 <- typeOf e0
    case t0 of
      TUni xs t1 ->
        case Vec.matchList xs ts1 of
          Nothing -> throwDocAt w
            ("expected" <+> int (length xs) <+> "type arguments, but found"
             <+> int (length ts1) <+> "type arguments")
          Just ts2 ->
            pure (t1 >>= scope (ts2 Vec.!) TVar)
      TFun{} ->
        throwErrorAt w "expected value argument, but found type argument"
      _ -> throwErrorAt w "unexpected type argument"

typeOfBranching ::
  (Typed st, IsTVar tv, IsEVar ev, HasPos branch) =>
  (Type tv -> branch -> TC tv ev (Type tv)) ->
  Expr st tv ev -> NE.NonEmpty branch -> TC tv ev (Type tv)
typeOfBranching typeOfBranch e0 (b1 NE.:| bs) = do
  t0 <- typeOf e0
  t1 <- typeOfBranch t0 b1
  for_ bs $ \b2 -> do
    t2 <- typeOfBranch t0 b2
    unless (t1 == t2) $
      throwDocAt (b2^.pos)
        ("expected type" <+> pretty t1 <> ", but found type" <+> pretty t2)
  pure t1

typeOfAltn ::
  (Typed st, IsTVar tv, IsEVar ev) =>
  Type tv -> Altn st tv ev -> TC tv ev (Type tv)
typeOfAltn t (MkAltn _ p e) = do
  env <- patnEnvLevel p t
  localizeTypes env (typeOf e)

typeOfCase ::
  (Typed st, IsTVar tv, IsEVar ev) =>
  Type tv -> Case st tv ev -> TC tv ev (Type tv)
typeOfCase t (MkCase w c ts bs e0) = do
  let f i = case bs Vec.! i of
        Nothing -> bug "reference to wildcard"
        Just x  -> x
      ps = map (maybe (PWld w) (PVar w)) (toList bs)
      e1 = fmap (first f) e0
  typeOfAltn t (MkAltn w (PCon w c ts ps) e1)

patnEnvLevel ::
  (IsTVar tv) =>
  Patn st tv -> Type tv -> TC tv ev (EnvLevelOf Id.EVar (Type tv))
patnEnvLevel p t0 = case p of
  PWld _   -> pure Map.empty
  PVar _ x -> pure (Map.singleton x t0)
  PCon w c _ts ps -> do
    Con.MkDConDecl (Con.MkDConDeclN tcon1 dcon1 _tag flds1) <- findDCon c
    case gatherTApp t0 of
      (TCon tcon0, ts0L)
        | tcon0 /= tcon1 -> throwDocAt w
          ("expected constructor of type" <+> pretty tcon0
           <> ", but found constructor of type" <+> pretty tcon1)
        | length flds1 /= length ps -> throwDocAt w
          ("expected" <+> int (length flds1) <+> "pattern arguments of" <+> pretty dcon1
           <> ", but found" <+> int (length ps) <+> "pattern arguments")
        | otherwise -> Vec.withList ts0L $ \ts0 ->
            let fldsProxy :: [Type (TFinScope n tv)] -> Proxy n
                fldsProxy _ = Proxy
            in  case sameNat (Vec.plength ts0) (fldsProxy flds1) of
                  Nothing -> bugWith "mismatching kinds for type constructor" tcon0
                  Just Refl -> do
                    let t_ps = map (>>= scope (ts0 Vec.!) absurd) flds1
                    Map.unions <$> zipWithM patnEnvLevel ps t_ps
      _ -> throwDocAt w ("cannot pattern match on type" <+> pretty t0)

check :: (Typed st, IsTVar tv, IsEVar ev) => Expr st tv ev -> Type tv -> TC tv ev ()
check e t0 = do
  t1 <- typeOf e
  unless (t0 == t1) $
    throwDocAt (e^.pos)
      ("expected type" <+> pretty t0 <> ", but found type" <+> pretty t1)

checkDefn :: (Typed st, IsEVar ev, IsTVar tv) => Defn st tv ev -> TC tv ev ()
checkDefn (MkDefn (MkBind _ _ t) e) = check e t

checkTopLevel :: (Typed st) => TopLevel st -> TC Void Void ()
checkTopLevel = \case
  TLDef   d -> checkDefn d
  TLAsm _ _ -> pure ()
