module Pukeko.Language.TypeResolver
  ( TR.Module
  , resolveModule
  )
where

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable       (for_)
import qualified Data.Map            as Map
import           Data.Maybe          (isJust)
import           Data.Traversable    (for)

import           Pukeko.Error
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.TypeResolver.AST as TR
import qualified Pukeko.Language.Renamer.AST      as Rn
import qualified Pukeko.Language.Type             as Ty
import qualified Pukeko.Language.Ident            as Id

data TRState = MkTRState
  { _typeCons :: Map.Map Id.Con TR.TypeCon
  , _exprCons :: Map.Map Id.Con TR.ExprCon
  }
makeLenses ''TRState

newtype TR a = TR {unTR :: ExceptT String (State TRState) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadState TRState
           )

runTR :: MonadError String m => TR a -> m a
runTR tr =
  let st = MkTRState{_typeCons = mempty, _exprCons = mempty}
  in  evalState (runExceptT (unTR tr)) st

-- TODO: Use @typeApps . _1@ to do this.
trType :: Pos -> Ty.Type Id.Con Ty.Closed -> TR (Ty.Type TR.TypeCon Ty.Closed)
trType w = Ty.type2con $ \con -> do
  adt_opt <- Map.lookup con <$> use typeCons
  case adt_opt of
    Nothing  -> throwAt w "unknown type cons" con
    Just adt -> pure adt

-- TODO: Have only one insert function.
insertTypeCon :: Pos -> TR.TypeCon -> TR ()
insertTypeCon posn adt@Ty.MkADT{_name} = do
  old <- use (typeCons . at _name)
  when (isJust old) $ throwAt posn "duplicate type cons" _name
  typeCons . at _name ?= adt

insertExprCon :: Pos -> TR.ExprCon -> TR ()
insertExprCon posn con@Ty.MkConstructor{_name} = do
  old <- use (exprCons . at _name)
  when (isJust old) $ throwAt posn "duplicate term cons" _name
  exprCons . at _name ?= con

findExprCon :: Pos -> Id.Con -> TR TR.ExprCon
findExprCon posn name = do
  con_opt <- Map.lookup name <$> use exprCons
  case con_opt of
    Nothing -> throwAt posn "unknown term cons" name
    Just con -> return con

trTopLevel :: Rn.TopLevel -> TR TR.TopLevel
trTopLevel top = case top of
  Rn.TypDef w adts -> do
    for_ adts (insertTypeCon w)
    adts <- for adts $ \_adt@Ty.MkADT{_constructors} -> do
      _constructors <- forM _constructors $ \con@Ty.MkConstructor{_fields} -> do
        _fields <- traverse (trType w) _fields
        let con' = con{Ty._adt, Ty._fields}
        insertExprCon w con'
        return con'
      return _adt{Ty._constructors}
    return (TR.TypDef w adts)
  Rn.Val w x t -> TR.Val w x <$> trType w t
  Rn.TopLet w ds -> TR.TopLet w <$> itraverseOf (traverse . defn2exprCon) findExprCon ds
  Rn.TopRec w ds -> TR.TopRec w <$> itraverseOf (traverse . defn2exprCon) findExprCon ds
  Rn.Asm w x a -> pure $ TR.Asm w x a

resolveModule :: MonadError String m => Rn.Module -> m TR.Module
resolveModule = runTR . traverse trTopLevel
