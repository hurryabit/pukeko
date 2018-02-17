{-# LANGUAGE ViewPatterns #-}
module Pukeko.FrontEnd.TypeResolver
  ( resolveModule
  ) where

import Pukeko.Prelude

import qualified Data.Map     as Map

import           Pukeko.AST.SystemF
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Type
import qualified Pukeko.AST.Identifier as Id

type In = Surface

data TRState = MkTRState
  { _st2tcons :: Map Id.TCon TConDecl
  , _st2dcons :: Map Id.DCon (TConDecl, DConDecl)
  }
makeLenses ''TRState

type TR = Eff [Reader SourcePos, State TRState, Error Failure]

evalTR :: TR a -> Either Failure a
evalTR =
  let st0 = MkTRState mempty mempty
  in  run . runError . evalState st0 . runReader noPos

-- TODO: Use proper terminology in error messages.
trType :: Type tv -> TR ()
trType = type2tcon_ $ \tcon -> do
  ex <- uses st2tcons (Map.member tcon)
  unless ex (throwHere ("unknown type constructor:" <+> pretty tcon))

insertTCon :: TConDecl -> TR ()
insertTCon = here' $ \tcon@MkTConDecl{_tcon2name = unlctd -> tname} -> do
  old <- uses st2tcons (Map.lookup tname)
  when (isJust old) $ throwHere ("duplicate type constructor:" <+> pretty tname)
  modifying st2tcons (Map.insert tname tcon)

insertDCon :: TConDecl -> DConDecl -> TR ()
insertDCon tcon dcon@MkDConDecl{_dcon2name = unlctd -> dname} = do
  old <- uses st2dcons (Map.lookup dname)
  when (isJust old) $ throwHere ("duplicate data constructor:" <+> pretty dname)
  modifying st2dcons (Map.insert dname (tcon, dcon))

findDCon :: Id.DCon -> TR Id.DCon
findDCon dcon = do
  ex <- uses st2dcons (Map.member dcon)
  unless ex (throwHere ("unknown data constructor:" <+> pretty dcon))
  pure dcon

-- TODO: Move this check into the renamer.
trDefn :: Defn In tv ev -> TR ()
trDefn = traverseOf_ (defn2expr . expr2atom . _ACon) findDCon

trDecl :: Decl In -> TR ()
trDecl top = case top of
  DType tcon@MkTConDecl{_tcon2dcons = dconDecls0} -> do
    insertTCon tcon
    case dconDecls0 of
        Left typ -> trType typ
        Right dconDecls ->
          for_ dconDecls $ here' $ \dcon@MkDConDecl{_dcon2flds = flds} -> do
            for_ flds trType
            insertDCon tcon dcon
  DSign (MkBind _ t) -> trType t
  -- FIXME: Resolve types in class declarations and instance definitions.
  DClss _ -> pure ()
  DInst i -> traverseOf_ inst2defn trDefn i
  DDefn d -> trDefn d
  DExtn _ -> pure ()

resolveModule :: Module In -> Either Failure ()
resolveModule (MkModule decls) = evalTR (traverse_ trDecl decls)
