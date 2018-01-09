module Pukeko.Language.TypeResolver
  ( resolveModule
  ) where

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable       (for_)
import qualified Data.Map            as Map
import           Data.Maybe          (isJust)
import           GHC.TypeLits        (KnownNat)

import           Pukeko.Error
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage        as St
import qualified Pukeko.Language.AST.ModuleInfo   as MI
import qualified Pukeko.Language.AST.ConDecl      as Con
import           Pukeko.Language.Type
import qualified Pukeko.Language.Ident            as Id

type In  = St.Renamer
type Out = St.TypeResolver

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

-- TODO: Use proper terminology in error messages.
trType :: Pos -> Type tv -> TR (Type tv)
trType w = type2tcon $ \tcon -> do
  ex <- uses st2tcons (has (ix tcon))
  unless ex (throwAt w "unknown type cons" tcon)
  pure tcon

insertTCon :: Pos -> Con.TConDecl -> TR ()
insertTCon posn tcon@Con.MkTConDecl{_tname} = do
  old <- use (st2tcons . at _tname)
  when (isJust old) $ throwAt posn "duplicate type cons" _tname
  st2tcons . at _tname ?= tcon

insertDCon :: KnownNat n => Pos -> Con.DConDeclN n -> TR ()
insertDCon posn con@Con.MkDConDeclN{_dname} = do
  old <- use (st2dcons . at _dname)
  when (isJust old) $ throwAt posn "duplicate term cons" _dname
  st2dcons . at _dname ?= Con.MkDConDecl con

findDCon :: Pos -> Id.DCon -> TR Id.DCon
findDCon w dcon = do
  ex <- uses st2dcons (has (ix dcon))
  unless ex (throwAt w "unknown term cons" dcon)
  pure dcon

trTopLevel :: TopLevel In -> TR (TopLevel Out)
trTopLevel top = case top of
  TLTyp w tconDecls -> do
    for_ tconDecls (insertTCon w)
    for_ tconDecls $ \Con.MkTConDecl{_dcons = dconDecls} -> do
      for_ dconDecls $ \dconDecl@Con.MkDConDeclN{_fields} -> do
        for_ _fields (trType w)
        insertDCon w dconDecl
    pure (TLTyp w tconDecls)
  TLVal w x t  -> TLVal w x <$> trType w t
  TLDef     d  -> TLDef <$> itraverseOf defn2dcon findDCon d
  TLAsm   b a  -> pure (TLAsm (retagBind b) a)

resolveModule :: MonadError String m => Module In -> m (Module Out)
resolveModule (MkModule _info0 tops0) = do
  (tops1, MkTRState tcons1 dcons1) <- runTR (traverse trTopLevel tops0)
  let info1 = MI.MkModuleInfo (MI.Present tcons1) (MI.Present dcons1) MI.Absent
  pure (MkModule info1 tops1)
