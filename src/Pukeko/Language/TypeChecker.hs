{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Pukeko.Language.TypeChecker
  ( TC.Module
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
import qualified Data.Vector.Sized as Vec

import           Pukeko.Error
import           Pukeko.Pos
import           Pukeko.Pretty
import           Pukeko.Language.AST.Classes
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Scope         as Sc
import qualified Pukeko.Language.KindChecker.AST   as KC
import qualified Pukeko.Language.TypeChecker.AST   as TC
import qualified Pukeko.Language.Ident             as Id
import qualified Pukeko.Language.Type              as Ty
import qualified Pukeko.Language.TypeChecker.Unify as U

type TypeClosed = Ty.Type TC.TypeCon  Ty.Closed
type TypeOpen s = Ty.Type TC.TypeCon (Ty.Open s)

data Environment v s = MkEnvironment
  { _locals :: Sc.EnvOf v (TypeOpen s)
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
  TC{unTC :: ReaderT (Environment v s) (StateT TCState (ExceptT String (ST s))) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (Environment v s)
           , MonadState TCState
           )

evalTC :: MonadError String m => (forall s. TC Id.EVar s a) -> m a
evalTC tc = runST $
  let env = MkEnvironment
        { _locals = mempty
        , _level  = 0
        }
      st = MkTCState
        { _declared = mempty
        , _defined  = mempty
        , _fresh    = []
        }
  in  runExceptT (evalStateT (runReaderT (unTC tc) env) st)

liftST :: ST s a -> TC v s a
liftST = TC . lift . lift . lift

resetFresh :: TC v s ()
resetFresh = fresh .= Id.freshTVars

freshUVar :: TC v s (TypeOpen s)
freshUVar = do
  _level <- view level
  _ident <- fresh %%= (\(x:xs) -> (x,xs))
  Ty.UVar <$> liftST (newSTRef Ty.Free{Ty._ident, Ty._level})

unify :: Pos -> TypeOpen s -> TypeOpen s -> TC v s ()
unify w t1 t2 = TC $ lift $ lift $ U.unify w t1 t2

localize ::
  forall i v s a.
  (IsVarLevel i, IsVar v) =>
  EnvLevelOf i (TypeOpen s) ->
  TC (Scope i v) s a ->
  TC v s a
localize ts = TC . withReaderT (locals %~ Sc.extendEnv @i @v ts) . unTC

lookupType :: (IsVar v, Pretty v) => Pos -> v -> TC v s (TypeOpen s)
lookupType w x = do
  env <- view locals
  case Sc.lookupEnv env x of
    Nothing -> throwAt w "undefined function" x
    Just t  -> return t

generalize :: TypeOpen s -> TC v s (TypeOpen s)
generalize = \case
  t@(Ty.UVar uref) -> do
    uvar <- liftST $ readSTRef uref
    cur_level <- view level
    case uvar of
      Ty.Free{Ty._ident, Ty._level}
        | _level > cur_level -> pure (Ty.Var _ident)
        | otherwise          -> pure t
      Ty.Link{Ty._type} -> generalize _type
  t@(Ty.Var _) -> pure t
  t@Ty.Arr     -> pure t
  t@(Ty.Con _) -> pure t
  Ty.App tf tp -> Ty.App <$> generalize tf <*> generalize tp

instantiate :: TypeOpen s -> TC v s (TypeOpen s)
instantiate t = do
  vars <- liftST $ Ty.openVars t
  env <- sequence $ Map.fromSet (const freshUVar) vars
  liftST $ Ty.openSubst env t

instantiateADT :: TC.TypeCon -> TC v s (TypeOpen s, Map Id.TVar (TypeOpen s))
instantiateADT adt@Ty.MkADT{Ty._params} = do
  t_params <- traverse (const freshUVar) _params
  return (Ty.appADT adt t_params, Map.fromList $ zip _params t_params)

inferPatn :: KC.Patn -> TypeOpen s -> TC v s (Map Id.EVar (TypeOpen s))
inferPatn patn t_expr = case patn of
  Bind (Wild _) -> return Map.empty
  Bind (Name _ ident) -> return (Map.singleton ident t_expr)
  Dest w con patns -> do
    let Ty.MkConstructor{Ty._name, Ty._adt, Ty._fields} = con
    when (length patns /= length _fields) $
      throwDocAt w $ "term cons" <+> quotes (pretty _name) <+>
      "expects" <+> int (length _fields) <+> "arguments"
    (t_adt, env_adt) <- instantiateADT _adt
    unify w t_expr t_adt
    t_fields <- liftST $ traverse (Ty.openSubst env_adt . Ty.open) _fields
    Map.unions <$> zipWithM inferPatn patns t_fields

-- TODO: Share mode core between 'inferLet' and 'inferRec'
-- TODO: Add test to ensure types are generalized properly.
inferLet
  :: (IsVar v)
  => Vec.Vector n (KC.Defn v) -> TC v s (Vec.Vector n (TypeOpen s))
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
  => Vec.Vector n (KC.Defn (FinScope n v)) -> TC v s (Vec.Vector n (TypeOpen s))
inferRec defns = do
  t_rhss <- local (level +~ 1) $ do
    t_lhss <- traverse (const freshUVar) defns
    t_rhss <- localize t_lhss $ traverse (infer . view rhs) defns
    ifor_ defns $ \i defn ->
      unify (defn^.pos) (t_lhss Vec.! i) (t_rhss Vec.! i)
    return t_rhss
  traverse generalize t_rhss

infer :: IsVar v => KC.Expr v -> TC v s (TypeOpen s)
infer expr = do
  let unifyHere t1 t2 = unify (expr^.pos) t1 t2
  case expr of
    Var w ident -> do
      t <- lookupType w ident
      instantiate t
    Con _ con -> instantiate $ Ty.open (Ty.typeOf con)
    Num _ _num -> return (Ty.open Ty.typeInt)
    App _ fun args -> do
      t_fun <- infer fun
      t_args <- traverse infer args
      t_res <- freshUVar
      unifyHere t_fun (t_args Ty.*~> t_res)
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
    -- KC.If{} -> bug "type checker" "if-then-esle" Nothing
    Mat _ expr altns -> do
      t_expr <- infer expr
      t_res <- freshUVar
      for_ altns $ \(MkAltn w patn rhs) -> do
        t_binds <- inferPatn patn t_expr
        t_rhs <- localize t_binds (infer rhs)
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

checkTopLevel :: KC.TopLevel -> TC Id.EVar s [TC.TopLevel]
checkTopLevel top = case top of
  KC.Val w ident type_ -> do
    isDeclared <- uses declared (ident `Map.member`)
    when isDeclared $ throwAt w "duplicate declaration of function" ident
    declared . at ident ?= type_
    return []
  KC.TopLet _ defns -> handleLetOrRec inferLet  retagExpr                 defns
  KC.TopRec _ defns -> handleLetOrRec inferRec (retagExpr . fmap unscope) defns
  KC.Asm w ident asm -> do
    t_decl <- ensureDefinable w ident
    define ident t_decl
    return [TC.Asm w ident asm]
  where
    handleLetOrRec inferLetOrRec mkTopExpr defns = do
      t_decls <- for defns $ \defn -> ensureDefinable (defn^.pos) (defn^.lhs)
      resetFresh
      env <- uses defined (fmap Ty.open)
      local (locals .~ env) $ do
        t_defns <- inferLetOrRec defns
        ifor_ defns $ \i defn -> do
          -- TODO: Check out if this is necessary.
          t_defn <- instantiate (t_defns Vec.! i)
          let t_decl = t_decls Vec.! i
          unify (defn^.pos) (Ty.open t_decl) t_defn
          define (defn^.lhs) t_decl
      return $ map (\(MkDefn w lhs rhs) -> TC.Def w lhs (mkTopExpr rhs)) (toList defns)

checkModule :: MonadError String m => KC.Module -> m TC.Module
checkModule module_ = evalTC $ concat <$> traverse checkTopLevel module_
