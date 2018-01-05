{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Pukeko.Language.TypeChecker
  ( checkModule
  )
where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.ST
import           Data.Foldable    (for_, toList)
import qualified Data.Map         as Map
import           Data.STRef
import           Data.Traversable
import qualified Data.Vector.Sized as Vec

import           Pukeko.Error
import           Pukeko.Pos
import           Pukeko.Pretty
import           Pukeko.Language.Info
import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage         as St
import qualified Pukeko.Language.AST.ConDecl       as Con
import qualified Pukeko.Language.AST.ModuleInfo    as MI
import qualified Pukeko.Language.AST.Scope         as Sc
import qualified Pukeko.Language.Ident             as Id
import qualified Pukeko.Language.Type              as Ty
import qualified Pukeko.Language.TypeChecker.Unify as U

type In  = St.KindChecker
type Out = St.TypeChecker

type TypeOpen s = Ty.Type (Ty.Open s)

data Environment v s = MkEnvironment
  { _locals :: Sc.EnvOf v (TypeOpen s)
  , _level  :: Int
  }
makeLenses ''Environment

newtype TCState = MkTCState{_fresh :: [Id.TVar]}
makeLenses ''TCState

newtype TC v s a =
  TC{unTC :: InfoT (ModuleInfo In) (ReaderT (Environment v s) (StateT TCState (ExceptT String (ST s)))) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (Environment v s)
           , MonadState TCState
           , MonadInfo (GenModuleInfo 'True 'True)
           )

evalTC ::
  MonadError String m =>
  (forall s. TC Id.EVar s a) -> ModuleInfo In -> m a
evalTC tc info = runST $
  let locals0 = Map.map (Ty.open . snd) (MI.funs info)
      env0 = MkEnvironment locals0 0
      st0 = MkTCState []
  in  runExceptT (evalStateT (runReaderT (runInfoT (unTC tc) info) env0) st0)

liftST :: ST s a -> TC v s a
liftST = TC . lift . lift . lift . lift

resetFresh :: TC v s ()
resetFresh = fresh .= Id.freshTVars

freshUVar :: TC v s (TypeOpen s)
freshUVar = do
  _level <- view level
  _ident <- fresh %%= (\(x:xs) -> (x,xs))
  Ty.UVar <$> liftST (newSTRef Ty.Free{_ident, _level})

unify :: Pos -> TypeOpen s -> TypeOpen s -> TC v s ()
unify w t1 t2 = TC $ lift $ lift $ lift $ U.unify w t1 t2

localize ::
  forall i v s a.
  (IsVarLevel i, IsVar v) =>
  EnvLevelOf i (TypeOpen s) ->
  TC (Scope i v) s a ->
  TC v s a
localize ts = TC . mapInfoT (withReaderT (locals %~ Sc.extendEnv @i @v ts)) . unTC

lookupType :: (IsVar v, Pretty v) => v -> TC v s (TypeOpen s)
lookupType = views locals . Sc.lookupEnv

generalize :: TypeOpen s -> TC v s (TypeOpen s)
generalize = \case
  t@(Ty.UVar uref) -> do
    uvar <- liftST $ readSTRef uref
    cur_level <- view level
    case uvar of
      Ty.Free{_ident, _level}
        | _level > cur_level -> pure (Ty.Var _ident)
        | otherwise          -> pure t
      Ty.Link{_type} -> generalize _type
  t@(Ty.Var _) -> pure t
  t@Ty.Arr     -> pure t
  t@(Ty.Con _) -> pure t
  Ty.App tf tp -> Ty.App <$> generalize tf <*> generalize tp

instantiate :: TypeOpen s -> TC v s (TypeOpen s)
instantiate t = do
  vars <- liftST $ Ty.openVars t
  env <- sequence $ Map.fromSet (const freshUVar) vars
  liftST $ Ty.openSubst env t

instantiateTCon :: Id.TCon -> TC v s (TypeOpen s, Map.Map Id.TVar (TypeOpen s))
instantiateTCon tcon = do
  Con.MkTConDecl{_params} <- findTCon tcon
  t_params <- traverse (const freshUVar) _params
  return (Ty.appTCon tcon t_params, Map.fromList $ zip _params t_params)

inferPatn :: Patn In -> TypeOpen s -> TC v s (Map.Map Id.EVar (TypeOpen s))
inferPatn patn t_expr = case patn of
  Bind (Wild _) -> return Map.empty
  Bind (Name _ ident) -> return (Map.singleton ident t_expr)
  Dest w dcon patns -> do
    Con.MkDConDecl{_dname, _tcon, _fields} <- findDCon dcon
    when (length patns /= length _fields) $
      throwDocAt w $ "term cons" <+> quotes (pretty _dname) <+>
      "expects" <+> int (length _fields) <+> "arguments"
    (t_inst, env_inst) <- instantiateTCon _tcon
    unify w t_expr t_inst
    t_fields <- liftST $ traverse (Ty.openSubst env_inst . Ty.open) _fields
    Map.unions <$> zipWithM inferPatn patns t_fields

-- TODO: Share mode core between 'inferLet' and 'inferRec'
-- TODO: Add test to ensure types are generalized properly.
inferLet
  :: (IsVar v)
  => Vec.Vector n (Defn In v) -> TC v s (Vec.Vector n (TypeOpen s))
inferLet defns = do
  t_rhss <- local (level +~ 1) $ do
    t_lhss <- traverse (const freshUVar) defns
    t_rhss <- traverse (infer . view rhs) defns
    ifor_ defns $ \i defn ->
      unify (defn^.pos) (t_lhss Vec.! i) (t_rhss Vec.! i)
    return t_rhss
  traverse generalize t_rhss

inferRec
  :: (IsVar v)
  => Vec.Vector n (Defn In (FinScope n v)) -> TC v s (Vec.Vector n (TypeOpen s))
inferRec defns = do
  t_rhss <- local (level +~ 1) $ do
    t_lhss <- traverse (const freshUVar) defns
    t_rhss <- localize t_lhss $ traverse (infer . view rhs) defns
    ifor_ defns $ \i defn ->
      unify (defn^.pos) (t_lhss Vec.! i) (t_rhss Vec.! i)
    return t_rhss
  traverse generalize t_rhss

infer :: IsVar v => Expr In v -> TC v s (TypeOpen s)
infer = \case
    Var _ ident -> do
      t <- lookupType ident
      instantiate t
    Con _ dcon -> do
      dconDecl <- findDCon dcon
      tconDecl <- findTCon (Con._tcon dconDecl)
      instantiate $ Ty.open (Con.typeOf tconDecl dconDecl)
    Num _ _num -> return (Ty.open Ty.typeInt)
    App w fun args -> do
      t_fun <- infer fun
      t_args <- traverse infer args
      t_res <- freshUVar
      unify w t_fun (t_args Ty.*~> t_res)
      return t_res
    Lam _ binds rhs -> do
      t_params <- traverse (const freshUVar) binds
      t_rhs <- localize t_params (infer rhs)
      return (toList t_params Ty.*~> t_rhs)
    Let _ defns rhs -> do
      t_defns <- inferLet defns
      localize t_defns (infer rhs)
    Rec _ defns rhs -> do
      t_defns <- inferRec defns
      localize t_defns (infer rhs)
    Mat _ expr altns -> do
      t_expr <- infer expr
      t_res <- freshUVar
      for_ altns $ \(MkAltn w patn rhs) -> do
        t_binds <- inferPatn patn t_expr
        t_rhs <- localize t_binds (infer rhs)
        unify w t_res t_rhs
      return t_res

checkTopLevel :: TopLevel In -> TC Id.EVar s [TopLevel Out]
checkTopLevel = \case
  TopLet _ defns -> handleLetOrRec inferLet  retagExpr                 defns
  TopRec _ defns -> handleLetOrRec inferRec (retagExpr . fmap unscope) defns
  Asm w ident asm -> pure [Asm w ident asm]
  where
    handleLetOrRec inferLetOrRec mkTopExpr defns = do
      resetFresh
      t_defns <- inferLetOrRec defns
      for (toList (Vec.zip defns t_defns)) $ \(MkDefn w x e, t_defn0) -> do
        t_defn <- instantiate t_defn0
        t_decl <- lookupType x
        unify w t_decl t_defn
        pure (Def w x (mkTopExpr e))

checkModule :: MonadError String m => Module In -> m (Module Out)
checkModule (MkModule decls tops)=
  MkModule decls <$> evalTC (concat <$> traverse checkTopLevel tops) decls
