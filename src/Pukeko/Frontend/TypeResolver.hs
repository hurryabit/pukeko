module Pukeko.FrontEnd.TypeResolver
  ( resolveModule
  ) where

import Pukeko.Prelude

import           Control.Lens

import           Pukeko.AST.SystemF
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Type
import qualified Pukeko.AST.Identifier as Id

type In  = St.Renamer
type Out = St.Renamer

data TRState = MkTRState
  { _st2tcons :: Map Id.TCon (Some1 TConDecl)
  , _st2dcons :: Map Id.DCon (Some1 (Pair1 TConDecl DConDecl))
  }
makeLenses ''TRState

newtype TR a = TR {unTR :: StateT TRState (HereT (Except Doc)) a}
  deriving ( Functor, Applicative, Monad
           , MonadError Doc
           , MonadState TRState
           , MonadHere
           )

evalTR :: TR a -> Either Doc a
evalTR tr =
  let st = MkTRState mempty mempty
  in  runExcept (runHereT (evalStateT (unTR tr) st))

-- TODO: Use proper terminology in error messages.
trType :: Type tv -> TR (Type tv)
trType = type2tcon $ \tcon -> do
  ex <- uses st2tcons (has (ix tcon))
  unless ex (throwHere ("unknown type constructor:" <+> pretty tcon))
  pure tcon

insertTCon :: TConDecl n -> TR ()
insertTCon tcon@MkTConDecl{_tcon2name = tname} = do
  old <- use (st2tcons . at tname)
  when (isJust old) $ throwHere ("duplicate type constructor:" <+> pretty tname)
  st2tcons . at tname ?= Some1 tcon

insertDCon :: KnownNat n => TConDecl n -> DConDecl n -> TR ()
insertDCon tcon dcon@MkDConDecl{_dcon2name = dname} = do
  old <- use (st2dcons . at dname)
  when (isJust old) $ throwHere ("duplicate data constructor:" <+> pretty dname)
  st2dcons . at dname ?= Some1 (Pair1 tcon dcon)

findDCon :: Id.DCon -> TR Id.DCon
findDCon dcon = do
  ex <- uses st2dcons (has (ix dcon))
  unless ex (throwHere ("unknown data constructor:" <+> pretty dcon))
  pure dcon

-- FIXME: Move this check into the renamer.
trDefn :: Defn In tv ev -> TR (Defn Out tv ev)
trDefn = traverseOf defn2dcon findDCon

trDecl :: Decl In -> TR (Decl Out)
trDecl top = case top of
  DType tconDecls -> do
    forHeres_ tconDecls (\(Some1 tcon) -> insertTCon tcon)
    forHeres_ tconDecls $ \(Some1 tcon@MkTConDecl{_tcon2dcons = dconDecls}) -> do
      forHeres_ dconDecls $ \dcon@MkDConDecl{_dcon2flds = flds} -> do
        for_ flds trType
        insertDCon tcon dcon
    pure (DType tconDecls)
  DSign (MkSignDecl x t)  -> DSign <$> MkSignDecl x <$> trType t
  -- FIXME: Resolve types in class declarations and instance definitions.
  DClss c -> pure (DClss c)
  DInst i -> DInst <$> inst2defn trDefn i
  DDefn d -> DDefn <$> trDefn d
  DPrim p -> pure (DPrim p)

resolveModule :: Module In -> Either Doc (Module Out)
resolveModule = evalTR . module2decls (traverseHeres trDecl)
