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
import           Pukeko.Language.Type              (TypeSchema (..), typeInt)
import           Pukeko.Language.TypeChecker.UType
import qualified Pukeko.Language.TypeChecker.Unify as U

type In  = St.KindChecker
type Out = St.TypeChecker

data Environment v s = MkEnvironment
  { _locals :: Sc.EnvOf v (UType s)
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
  -- TODO: Remove this @fmap baseName@ hack.
  let locals0 = Map.map (\(_, MkTypeSchema _ t) -> open (fmap baseName t)) (MI.funs info)
      env0 = MkEnvironment locals0 0
      st0 = MkTCState []
  in  runExceptT (evalStateT (runReaderT (runInfoT (unTC tc) info) env0) st0)

liftST :: ST s a -> TC v s a
liftST = TC . lift . lift . lift . lift

resetFresh :: TC v s ()
resetFresh = fresh .= Id.freshTVars

freshUVar :: TC v s (UType s)
freshUVar = do
  x <- fresh %%= (\(x:xs) -> (x,xs))
  l <- view level
  UVar <$> liftST (newSTRef (UFree x l))

unify :: Pos -> UType s -> UType s -> TC v s ()
unify w t1 t2 = TC $ lift $ lift $ lift $ U.unify w t1 t2

localize ::
  forall i v s a.
  (IsVarLevel i, IsVar v) =>
  EnvLevelOf i (UType s) ->
  TC (EScope i v) s a ->
  TC v s a
localize ts = TC . mapInfoT (withReaderT (locals %~ Sc.extendEnv @i @v ts)) . unTC

lookupType :: (IsVar v, Pretty v) => v -> TC v s (UType s)
lookupType = views locals . Sc.lookupEnv

generalize :: UType s -> TC v s (UType s)
generalize t0 = case t0 of
  (UVar uref) -> do
    uvar <- liftST $ readSTRef uref
    cur_level <- view level
    case uvar of
      UFree x l
        | l > cur_level -> pure (UTVar x)
        | otherwise     -> pure t0
      ULink t1 -> generalize t1
  UTVar{} -> pure t0
  UTArr{} -> pure t0
  UTCon{} -> pure t0
  UTApp tf tp -> UTApp <$> generalize tf <*> generalize tp

instantiate :: UType s -> TC v s (UType s)
instantiate t = do
  xs <- liftST $ vars t
  env <- sequence $ Map.fromSet (const freshUVar) xs
  liftST $ subst env t

instantiateTCon :: Id.TCon -> TC v s (UType s, Map.Map Id.TVar (UType s))
instantiateTCon tcon = do
  Con.MkTConDecl{_params} <- findTCon tcon
  let params = toList _params
  t_params <- traverse (const freshUVar) params
  return (appTCon tcon t_params, Map.fromList (zip params t_params))

inferPatn :: Patn -> UType s -> TC v s (Map.Map Id.EVar (UType s))
inferPatn patn t_expr = case patn of
  PVar (BWild _) -> return Map.empty
  PVar (BName _ ident) -> return (Map.singleton ident t_expr)
  PCon w dcon patns -> do
    Con.MkDConDecl Con.MkDConDeclN{_dname, _tcon, _fields} <- findDCon dcon
    when (length patns /= length _fields) $
      throwDocAt w $ "term cons" <+> quotes (pretty _dname) <+>
      "expects" <+> int (length _fields) <+> "arguments"
    (t_inst, env_inst) <- instantiateTCon _tcon
    unify w t_expr t_inst
    t_fields <- liftST $ traverse (subst env_inst . open . fmap baseName) _fields
    Map.unions <$> zipWithM inferPatn patns t_fields

-- TODO: Share mode core between 'inferLet' and 'inferRec'
-- TODO: Add test to ensure types are generalized properly.
inferLet
  :: (IsEVar v)
  => Vec.Vector n (Defn In v) -> TC v s (Vec.Vector n (UType s))
inferLet defns = do
  t_rhss <- local (level +~ 1) $ do
    t_lhss <- traverse (const freshUVar) defns
    t_rhss <- traverse (infer . view rhs) defns
    ifor_ defns $ \i defn ->
      unify (defn^.pos) (t_lhss Vec.! i) (t_rhss Vec.! i)
    return t_rhss
  traverse generalize t_rhss

inferRec
  :: (IsEVar v)
  => Vec.Vector n (Defn In (EFinScope n v)) -> TC v s (Vec.Vector n (UType s))
inferRec defns = do
  t_rhss <- local (level +~ 1) $ do
    t_lhss <- traverse (const freshUVar) defns
    t_rhss <- localize t_lhss $ traverse (infer . view rhs) defns
    ifor_ defns $ \i defn ->
      unify (defn^.pos) (t_lhss Vec.! i) (t_rhss Vec.! i)
    return t_rhss
  traverse generalize t_rhss

infer :: IsEVar v => Expr In v -> TC v s (UType s)
infer = \case
    EVar _ ident -> do
      t <- lookupType ident
      instantiate t
    ECon _ dcon -> do
      dconDecl@(Con.MkDConDecl Con.MkDConDeclN{_tcon}) <- findDCon dcon
      tconDecl <- findTCon _tcon
      instantiate $ open (Con.typeOf tconDecl dconDecl)
    ENum _ _num -> return (open typeInt)
    EApp w fun args -> do
      t_fun <- infer fun
      t_args <- traverse infer args
      t_res <- freshUVar
      unify w t_fun (t_args *~> t_res)
      return t_res
    ELam _ binds rhs -> do
      t_params <- traverse (const freshUVar) binds
      t_rhs <- localize t_params (infer rhs)
      return (toList t_params *~> t_rhs)
    ELet _ defns rhs -> do
      t_defns <- inferLet defns
      localize t_defns (infer rhs)
    ERec _ defns rhs -> do
      t_defns <- inferRec defns
      localize t_defns (infer rhs)
    EMat _ expr altns -> do
      t_expr <- infer expr
      t_res <- freshUVar
      for_ altns $ \(MkAltn w patn rhs) -> do
        t_binds <- inferPatn patn t_expr
        t_rhs <- localize t_binds (infer rhs)
        unify w t_res t_rhs
      return t_res

checkTopLevel :: TopLevel In -> TC Id.EVar s [TopLevel Out]
checkTopLevel = \case
  TLLet _ defns -> handleLetOrRec inferLet  retagExpr                 defns
  TLRec _ defns -> handleLetOrRec inferRec (retagExpr . fmap unscope) defns
  TLAsm w ident asm -> pure [TLAsm w ident asm]
  where
    handleLetOrRec inferLetOrRec mkTopExpr defns = do
      resetFresh
      t_defns <- inferLetOrRec defns
      for (toList (Vec.zip defns t_defns)) $ \(MkDefn w x e, t_defn0) -> do
        t_defn <- instantiate t_defn0
        t_decl <- lookupType x
        unify w t_decl t_defn
        pure (TLDef w x (mkTopExpr e))

checkModule :: MonadError String m => Module In -> m (Module Out)
checkModule (MkModule decls tops)=
  MkModule decls <$> evalTC (concat <$> traverse checkTopLevel tops) decls
