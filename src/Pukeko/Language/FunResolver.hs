module Pukeko.Language.FunResolver
  ( resolveModule
  ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable (traverse_)
import qualified Data.Map      as Map
import           Data.Maybe    (catMaybes)
import qualified Data.Set      as Set

import           Pukeko.Error
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage      as St
import qualified Pukeko.Language.AST.ModuleInfo as MI
import qualified Pukeko.Language.Ident          as Id
import qualified Pukeko.Language.Type           as Ty

type In  = St.KindChecker
type Out = St.FunResolver

type TypeClosed = Ty.Type Ty.Closed

data FRState = MkFRState
  { _declared :: Map.Map Id.EVar (Pos, TypeClosed)
  , _defined  :: Set.Set Id.EVar
  }
makeLenses ''FRState

newtype FR a = FR{unFR :: StateT FRState (Except String) a}
  deriving ( Functor, Applicative, Monad
           , MonadState FRState
           , MonadError String
           )

runFR :: MonadError String m => FR a -> m (a, FRState)
runFR fr = runExcept (runStateT (unFR fr) st0)
  where
    st0 = MkFRState mempty mempty

resolveModule :: MonadError String m => Module In -> m (Module Out)
resolveModule (MkModule info0 tops0) = do
  (tops1, MkFRState decld defnd) <- runFR (catMaybes <$> traverse frTopLevel tops0)
  let undefnd = decld `Map.difference` Map.fromSet id defnd
  case Map.minViewWithKey undefnd of
    Just ((fun, (w, _)), _) -> throwAt w "declared but undefined function" fun
    Nothing -> do
      let info1 = info0{MI._funs = MI.Present decld}
      pure (MkModule info1 tops1)

declareFun :: Pos -> Id.EVar -> TypeClosed -> FR ()
declareFun w fun typ = do
  dup <- uses declared (has (ix fun))
  when dup (throwAt w "duplicate declaration of function" fun)
  declared . at fun ?= (w, typ)

defineFun :: Pos -> Id.EVar -> FR ()
defineFun w fun = do
  ex <- uses declared (has (ix fun))
  unless ex (throwAt w "undeclared function" fun)
  dup <- use (defined . contains fun)
  when dup (throwAt w "duplicate definition of function" fun)
  defined . contains fun .= True

useFun :: Pos -> Id.EVar -> FR Id.EVar
useFun w fun = do
  ex <- use (defined . contains fun)
  unless ex (throwAt w "undefined function" fun)
  pure fun

frTopLevel :: TopLevel In -> FR (Maybe (TopLevel Out))
frTopLevel = \case
  Val w x t -> declareFun w x t *> pure Nothing
  TopLet w ds0 -> do
    ds1 <- traverse (itraverse useFun) ds0
    traverse_ defineFun' ds1
    yield (TopLet w (fmap retagDefn ds1))
  TopRec w ds0 -> do
    traverse_ defineFun' ds0
    ds1 <- traverse (itraverse (traverse . useFun)) ds0
    yield (TopRec w (fmap retagDefn ds1))
  Asm w x s -> defineFun w x *> yield (Asm w x s)
  where
    yield = pure . Just
    defineFun' (MkDefn w fun _) = defineFun w fun
