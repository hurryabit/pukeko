module Pukeko.Language.KindChecker
  ( check
  )
where

import Control.Monad
import Data.Maybe (catMaybes)
import Text.Parsec (SourcePos)
import qualified Data.Set as Set

import Pukeko.Error
import Pukeko.Pretty
import Pukeko.Language.Syntax
import Pukeko.Language.Type hiding (Type)
import qualified Pukeko.Language.Ident   as Ident
import qualified Pukeko.Language.Rewrite as Rewrite
import qualified Pukeko.Language.Type    as Type

type Type a = Type.Type (ADT Ident.Con) a

type KC a = Except String a

kcType :: SourcePos -> Type a -> KC (Type a)
kcType posn typ = case typ of
    TApp MkADT{_name, _params} typs
      -- TODO: Check that params are mutually distinct.
      | length _params /= length typs ->
          throwDocAt posn $
          "type cons" <+> quotes (pretty _name) <+> "expects" <+>
          int (length _params) <+> "parameters"
    _ -> Rewrite.type_ (kcType posn) typ

kcTopLevel :: TopLevel StageTR SourcePos -> KC (Maybe (TopLevel StageTR SourcePos))
kcTopLevel top = case top of
  Type{_annot, _adts} -> do
    forM_ _adts $ \MkADT{_params, _constructors} -> do
      forM_ _constructors $ \MkConstructor{_name, _fields} -> do
        let unbound =
              Set.unions (map qvars _fields) `Set.difference` Set.fromList _params
        unless (Set.null unbound) $
          throwAt _annot "unbound type vars in term cons" _name
        mapM_ (kcType _annot) _fields
    return Nothing
  Val{_annot, _type} -> do
    _type <- kcType _annot _type
    return $ Just (top{_type} :: TopLevel StageTR _)
  Def{} -> return $ Just top
  Asm{} -> return $ Just top

kcModule :: Module StageTR SourcePos -> KC (Module StageTR SourcePos)
kcModule module_ = catMaybes <$> traverse kcTopLevel module_

check :: MonadError String m => Module StageTR SourcePos -> m (Module StageTR SourcePos)
check = runExcept . kcModule
