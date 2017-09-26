{-# LANGUAGE RankNTypes, TemplateHaskell #-}
module Pukeko.Language.TypeChecker
  ( Module
  , checkModule
  )
where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.ST
import           Data.Foldable    (for_, toList)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe       (isJust)
import           Data.STRef
import           Data.Traversable (for)
import qualified Data.Vector.Sized as V

import           Pukeko.Error
import           Pukeko.Pretty
import           Pukeko.Language.Base.AST        hiding (Bound, Free)
import           Pukeko.Language.TypeChecker.AST (TypeCon, ExprCon, Module, TopLevel (..))
import qualified Pukeko.Language.KindChecker.AST as K
import qualified Pukeko.Language.Ident           as Id
import           Pukeko.Language.Type
import qualified Pukeko.Language.TypeChecker.Unify as U

type TypeClosed = Type TypeCon Closed
type TypeOpen s = Type TypeCon (Open s)

data Environment v s = MkEnvironment
  { _locals :: Map v (TypeOpen s)
  , _level  :: Int
  }
makeLenses ''Environment

data TCState = MkTCState
  { _declared :: Map Id.EVar TypeClosed
    -- TODO: Store open types in _defined.
  , _defined  :: Map Id.EVar TypeClosed
  , _fresh    :: [Id.TVar]
  }
makeLenses ''TCState

newtype TC v s a =
  TC {unTC :: ReaderT (Environment v s) (StateT TCState (ExceptT String (ST s))) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (Environment v s)
           , MonadState TCState
           )

evalTC :: MonadError String m => (forall s. TC Id.EVar s a) -> m a
evalTC tc =
  let env = MkEnvironment
        { _locals = mempty
        , _level  = 0
        }
      st = MkTCState
        { _declared = mempty
        , _defined  = mempty
        , _fresh    = []
        }
  in  runST (runExceptT (evalStateT (runReaderT (unTC tc) env) st))

liftST :: ST s a -> TC v s a
liftST = TC . lift . lift . lift

resetFresh :: TC v s ()
resetFresh = fresh .= Id.freshTVars

freshUVar :: TC v s (TypeOpen s)
freshUVar = do
  _level <- view level
  _ident <- fresh %%= (\(x:xs) -> (x,xs))
  UVar <$> liftST (newSTRef Free{_ident, _level})

unify :: Pos -> TypeOpen s -> TypeOpen s -> TC v s ()
unify w t1 t2 = TC $ lift $ lift $ U.unify w t1 t2

defnName :: K.Defn v -> Maybe Id.EVar
defnName = Just . view lhs

localize
  :: (IsVar v)
  => (a -> Maybe Id.EVar)
  -> Vector n a
  -> Vector n (TypeOpen s)
  -> TC (FinScope n v) s b
  -> TC v s b
localize name xs ts (TC tc) = TC $ withReaderT (locals %~ upd) tc
  where
    f i x = case name x of
      Nothing -> mempty
      Just y  -> Map.singleton (bound i y) (ts V.! i)
    upd old = ifoldMap f xs <> Map.mapKeysMonotonic free old

lookupType :: (Ord v, Pretty v) => Pos -> v -> TC v s (TypeOpen s)
lookupType w ident = do
  t_local <- view (locals . at ident)
  case t_local of
    Nothing -> throwAt w "undefined function" ident
    Just t  -> return t

generalize :: TypeOpen s -> TC v s (TypeOpen s)
generalize t = case t of
  UVar uref -> do
    uvar <- liftST $ readSTRef uref
    cur_level <- view level
    case uvar of
      Free{_ident, _level}
        | _level > cur_level -> return (Var _ident)
        | otherwise          -> return t
      Link{ _type } -> generalize _type
  Var _ -> return t
  Fun tx ty -> Fun <$> generalize tx <*> generalize ty
  App c  ts -> App c <$> traverse generalize ts

instantiate :: TypeOpen s -> TC v s (TypeOpen s)
instantiate t = do
  vars <- liftST $ openVars t
  env <- sequence $ Map.fromSet (const freshUVar) vars
  liftST $ openSubst env t

instantiateADT :: TypeCon -> TC v s (TypeOpen s, Map Id.TVar (TypeOpen s))
instantiateADT adt@MkADT{_params} = do
  t_params <- traverse (const freshUVar) _params
  return (app adt t_params, Map.fromList $ zip _params t_params)

-- inferPatn :: K.Patn -> TypeOpen s -> TC v s (Map Id.EVar (TypeOpen s))
-- inferPatn patn t_expr = case patn of
--   Wild _ -> return Map.empty
--   Bind _ ident -> return (Map.singleton ident t_expr)
--   Dest w con patns -> do
--     let MkConstructor{_name, _adt, _fields} = con
--     when (length patns /= length _fields) $
--       throwDocAt w $ "term cons" <+> quotes (pretty _name) <+>
--       "expects" <+> int (length _fields) <+> "arguments"
--     (t_adt, env_adt) <- instantiateADT _adt
--     unify w t_expr t_adt
--     t_fields <- liftST $ traverse (openSubst env_adt . open) _fields
--     Map.unions <$> zipWithM inferPatn patns t_fields

inferPatn
  :: Pos
  -> ExprCon
  -> Vector n Bind
  -> TypeOpen s
  -> TC v s (Vector n (TypeOpen s))
inferPatn w MkConstructor{_name, _adt, _fields} binds t_expr = do
  case V.matchList binds _fields of
    Nothing ->
      throwDocAt w $ "term cons" <+> quotes (pretty _name) <+>
      "expects" <+> int (length _fields) <+> "arguments"
    Just fields ->  do
      (t_adt, env_adt) <- instantiateADT _adt
      unify w t_expr t_adt
      liftST $ traverse (openSubst env_adt . open) fields

-- TODO: Share mode core between 'inferLet' and 'inferRec'
-- TODO: Add test to ensure types are generalized properly.
inferLet
  :: (IsVar v)
  => Vector n (K.Defn v) -> TC v s (Vector n (TypeOpen s))
inferLet defns = do
  t_rhss <- local (level +~ 1) $ do
    t_lhss <- traverse (const freshUVar) defns
    t_rhss <- traverse (infer . view rhs) defns
    ifor_ defns $ \i defn ->
      unify (defn^.pos) (t_lhss V.! i) (t_rhss V.! i)
    return t_rhss
  traverse generalize t_rhss

inferRec
  :: (IsVar v)
  => Vector n (K.Defn (FinScope n v)) -> TC v s (Vector n (TypeOpen s))
inferRec defns = do
  t_rhss <- local (level +~ 1) $ do
    t_lhss <- traverse (const freshUVar) defns
    t_rhss <- localize defnName defns t_lhss $ traverse (infer . view rhs) defns
    ifor_ defns $ \i defn ->
      unify (defn^.pos) (t_lhss V.! i) (t_rhss V.! i)
    return t_rhss
  traverse generalize t_rhss

infer :: IsVar v => K.Expr v -> TC v s (TypeOpen s)
infer expr = do
  let unifyHere t1 t2 = unify (expr^.pos) t1 t2
  case expr of
    K.Var w ident -> do
      t <- lookupType w ident
      instantiate t
    K.Con _ con -> instantiate $ open (typeOf con)
    K.Num _ _num -> return (open typeInt)
    K.App _ fun args -> do
      t_fun <- infer fun
      t_args <- traverse infer args
      t_res <- freshUVar
      unifyHere t_fun (t_args *~> t_res)
      return t_res
    K.Lam _ binds rhs -> do
      t_params <- traverse (const freshUVar) binds
      t_rhs <- localize bindName binds t_params (infer rhs)
      return (toList t_params *~> t_rhs)
    K.Let _ defns rhs -> do
      t_defns <- inferLet defns
      localize defnName defns t_defns (infer rhs)
    K.Rec _ defns rhs -> do
      t_defns <- inferRec defns
      localize defnName defns t_defns (infer rhs)
    -- K.If{} -> bug "type checker" "if-then-esle" Nothing
    K.Mat _ expr altns -> do
      t_expr <- infer expr
      t_res <- freshUVar
      for_ altns $ \(MkAltn w con binds rhs) -> do
        t_binds <- inferPatn w con binds t_expr
        t_rhs <- localize bindName binds t_binds (infer rhs)
        unify w t_res t_rhs
      return t_res

ensureDefinable :: Pos -> Id.EVar -> TC v s TypeClosed
ensureDefinable w ident = do
  mbt_decl <- use (declared . at ident)
  case mbt_decl of
    Nothing -> throwAt w "undeclared function" ident
    Just t_decl -> do
      mbt_defn  <- use (defined . at ident)
      when (isJust mbt_defn) $ throwAt w "duplicate definition of function" ident
      return t_decl

define :: Id.EVar -> TypeClosed -> TC v s ()
define ident type_ = defined . at ident ?= type_

checkTopLevel :: K.TopLevel -> TC Id.EVar s [TopLevel]
checkTopLevel top = case top of
  K.Val w ident type_ -> do
    isDeclared <- uses declared (ident `Map.member`)
    when isDeclared $ throwAt w "duplicate declaration of function" ident
    declared . at ident ?= type_
    return []
  K.TopLet _ defns -> handleLetOrRec inferLet id             defns
  K.TopRec _ defns -> handleLetOrRec inferRec (fmap unscope) defns
  K.Asm w ident asm -> do
    t_decl <- ensureDefinable w ident
    define ident t_decl
    return [Asm w ident asm]
  where
    handleLetOrRec inferLetOrRec mkTopExpr defns = do
      t_decls <- for defns $ \defn -> ensureDefinable (defn^.pos) (defn^.lhs)
      resetFresh
      env <- uses defined (fmap open)
      local (locals .~ env) $ do
        t_defns <- inferLetOrRec defns
        ifor_ defns $ \i defn -> do
          -- TODO: Check out if this is necessary.
          t_defn <- instantiate (t_defns V.! i)
          let t_decl = t_decls V.! i
          unify (defn^.pos) (open t_decl) t_defn
          define (defn^.lhs) t_decl
      return $ map (\(MkDefn w lhs rhs) -> Def w lhs (mkTopExpr rhs)) (toList defns)

checkModule :: MonadError String m => K.Module -> m Module
checkModule module_ = evalTC $ concat <$> traverse checkTopLevel module_
