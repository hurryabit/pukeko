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
trType w = Ty.type2tcon $ \tname -> do
  tcon_mb <- use (st2tcons . at tname)
  case tcon_mb of
    Nothing   -> throwAt w "unknown type cons" tname
    Just tcon -> pure tcon

-- TODO: Have only one insert function.
insertTCon :: Pos -> TR.TCon -> TR ()
insertTCon posn tcon@Ty.MkTConDecl{_tname} = do
  old <- use (st2tcons . at _tname)
  when (isJust old) $ throwAt posn "duplicate type cons" _tname
  st2tcons . at _tname ?= tcon

insertDCon :: Pos -> TR.DCon -> TR ()
insertDCon posn con@Ty.MkDConDecl{_dname} = do
  old <- use (st2dcons . at _dname)
  when (isJust old) $ throwAt posn "duplicate term cons" _dname
  st2dcons . at _dname ?= con

findDCon :: Pos -> Id.DCon -> TR TR.DCon
findDCon posn name = do
  con_opt <- Map.lookup name <$> use st2dcons
  case con_opt of
    Nothing -> throwAt posn "unknown term cons" name
    Just con -> return con

trTopLevel :: Rn.TopLevel -> TR TR.TopLevel
trTopLevel top = case top of
  Rn.TypDef w tcons -> do
    for_ tcons (insertTCon w)
    tcons <- for tcons $ \_tcon@Ty.MkTConDecl{_dcons} -> do
      _dcons <- forM _dcons $ \con@Ty.MkDConDecl{_fields} -> do
        _fields <- traverse (trType w) _fields
        let con' = con{Ty._tcon, Ty._fields}
        insertDCon w con'
        return con'
      return _tcon{Ty._dcons}
    return (TR.TypDef w tcons)
  Rn.Val w x t -> TR.Val w x <$> trType w t
  Rn.TopLet w ds -> TR.TopLet w <$> itraverseOf (traverse . defn2dcon) findDCon ds
  Rn.TopRec w ds -> TR.TopRec w <$> itraverseOf (traverse . defn2dcon) findDCon ds
  Rn.Asm w x a -> pure $ TR.Asm w x a

resolveModule :: MonadError String m => Rn.Module -> m TR.Module
resolveModule = runTR . traverse trTopLevel
