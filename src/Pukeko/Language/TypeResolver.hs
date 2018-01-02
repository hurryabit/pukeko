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

import           Pukeko.Error
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.ConDecl      as Con
import qualified Pukeko.Language.TypeResolver.AST as TR
import qualified Pukeko.Language.Renamer.AST      as Rn
import qualified Pukeko.Language.Type             as Ty
import qualified Pukeko.Language.Ident            as Id

data TRState = MkTRState
  { _st2tcons :: Map.Map Id.TCon Con.TConDecl
  , _st2dcons :: Map.Map Id.DCon Con.DConDecl
  }
makeLenses ''TRState

newtype TR a = TR {unTR :: StateT TRState (Except String) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadState TRState
           )

runTR :: MonadError String m => TR a -> m (a, TRState)
runTR tr =
  let st = MkTRState mempty mempty
  in  runExcept (runStateT (unTR tr) st)

-- TODO: Use @typeApps . _1@ to do this.
trType :: Pos -> Ty.Type Ty.Closed -> TR (Ty.Type Ty.Closed)
trType w = Ty.type2tcon $ \tcon -> do
  ex <- uses st2tcons (has (ix tcon))
  unless ex (throwAt w "unknown type cons" tcon)
  pure tcon

-- TODO: Have only one insert function.
insertTCon :: Pos -> Con.TConDecl -> TR ()
insertTCon posn tcon@Con.MkTConDecl{_tname} = do
  old <- use (st2tcons . at _tname)
  when (isJust old) $ throwAt posn "duplicate type cons" _tname
  st2tcons . at _tname ?= tcon

insertDCon :: Pos -> Con.DConDecl -> TR ()
insertDCon posn con@Con.MkDConDecl{_dname} = do
  old <- use (st2dcons . at _dname)
  when (isJust old) $ throwAt posn "duplicate term cons" _dname
  st2dcons . at _dname ?= con

findDCon :: Pos -> Id.DCon -> TR Id.DCon
findDCon w dcon = do
  ex <- uses st2dcons (has (ix dcon))
  unless ex (throwAt w "unknown term cons" dcon)
  pure dcon

trTopLevel :: Rn.TopLevel -> TR TR.TopLevel
trTopLevel top = case top of
  Rn.TypDef w tconDecls -> do
    for_ tconDecls (insertTCon w)
    for_ tconDecls $ \Con.MkTConDecl{_dcons = dconDecls} -> do
      for_ dconDecls $ \dconDecl@Con.MkDConDecl{_fields} -> do
        for_ _fields (trType w)
        insertDCon w dconDecl
    pure (TR.TypDef w tconDecls)
  Rn.Val w x t -> TR.Val w x <$> trType w t
  Rn.TopLet w ds -> TR.TopLet w <$> itraverseOf (traverse . defn2dcon) findDCon ds
  Rn.TopRec w ds -> TR.TopRec w <$> itraverseOf (traverse . defn2dcon) findDCon ds
  Rn.Asm w x a -> pure $ TR.Asm w x a

resolveModule :: MonadError String m => Rn.Module -> m TR.Module
resolveModule tops0 = do
  (tops1, MkTRState tcons1 dcons1) <- runTR (traverse trTopLevel tops0)
  pure (MkModule (Con.mkConDecls tcons1 dcons1) tops1)
