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
  { _st2tcons :: Map.Map Id.TCon TR.TCon
  , _st2dcons :: Map.Map Id.DCon TR.DCon
  }
makeLenses ''TRState

newtype TR a = TR {unTR :: ExceptT String (State TRState) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadState TRState
           )

runTR :: MonadError String m => TR a -> m a
runTR tr =
  let st = MkTRState mempty mempty
  in  evalState (runExceptT (unTR tr)) st

-- TODO: Use @typeApps . _1@ to do this.
trType :: Pos -> Ty.Type Id.TCon Ty.Closed -> TR (Ty.Type TR.TCon Ty.Closed)
trType w = Ty.type2con $ \con -> do
  adt_opt <- Map.lookup con <$> use st2tcons
  case adt_opt of
    Nothing  -> throwAt w "unknown type cons" con
    Just adt -> pure adt

-- TODO: Have only one insert function.
insertTCon :: Pos -> TR.TCon -> TR ()
insertTCon posn adt@Ty.MkADT{_name} = do
  old <- use (st2tcons . at _name)
  when (isJust old) $ throwAt posn "duplicate type cons" _name
  st2tcons . at _name ?= adt

insertDCon :: Pos -> TR.DCon -> TR ()
insertDCon posn con@Ty.MkConstructor{_name} = do
  old <- use (st2dcons . at _name)
  when (isJust old) $ throwAt posn "duplicate term cons" _name
  st2dcons . at _name ?= con

findDCon :: Pos -> Id.DCon -> TR TR.DCon
findDCon posn name = do
  con_opt <- Map.lookup name <$> use st2dcons
  case con_opt of
    Nothing -> throwAt posn "unknown term cons" name
    Just con -> return con

trTopLevel :: Rn.TopLevel -> TR TR.TopLevel
trTopLevel top = case top of
  Rn.TypDef w adts -> do
    for_ adts (insertTCon w)
    adts <- for adts $ \_adt@Ty.MkADT{_constructors} -> do
      _constructors <- forM _constructors $ \con@Ty.MkConstructor{_fields} -> do
        _fields <- traverse (trType w) _fields
        let con' = con{Ty._adt, Ty._fields}
        insertDCon w con'
        return con'
      return _adt{Ty._constructors}
    return (TR.TypDef w adts)
  Rn.Val w x t -> TR.Val w x <$> trType w t
  Rn.TopLet w ds -> TR.TopLet w <$> itraverseOf (traverse . defn2dcon) findDCon ds
  Rn.TopRec w ds -> TR.TopRec w <$> itraverseOf (traverse . defn2dcon) findDCon ds
  Rn.Asm w x a -> pure $ TR.Asm w x a

resolveModule :: MonadError String m => Rn.Module -> m TR.Module
resolveModule = runTR . traverse trTopLevel
