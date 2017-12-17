-- | Check that all type constructor are applied to the right number of
-- variables and all variables are bound.
module Pukeko.Language.KindChecker
  ( KC.Module
  , checkModule
  )
where

import           Control.Lens
import           Control.Monad (unless, when)
import           Data.Foldable
import           Data.Maybe    (catMaybes)
import qualified Data.Set      as Set

import           Pukeko.Error
import           Pukeko.Pretty
import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.TypeResolver.AST as TR
import qualified Pukeko.Language.KindChecker.AST  as KC
import qualified Pukeko.Language.Type             as Ty

type Type = Ty.Type KC.TypeCon Ty.Closed

type KC a = Except String a

kcType :: Pos -> Type -> KC ()
kcType w = itraverseOf_ Ty.typeCons $ \typs Ty.MkADT{_name, _params} -> do
  -- TODO: Check that params are mutually distinct.
  when (length _params /= length typs) $ throwDocAt w $
    "type cons" <+> quotes (pretty _name) <+> "expects" <+>
    int (length _params) <+> "parameters"

kcTopLevel :: TR.TopLevel -> KC (Maybe KC.TopLevel)
kcTopLevel = \case
  TR.TypDef w adts -> do
    for_ adts $ \Ty.MkADT{_params, _constructors} -> do
      for_ _constructors $ \Ty.MkConstructor{_name, _fields} -> do
        let unbound =
              Set.unions (map Ty.vars _fields) `Set.difference` Set.fromList _params
        unless (Set.null unbound) $ throwAt w "unbound type vars in term cons" _name
        traverse_ (kcType w) _fields
    return Nothing
  TR.Val    w x  t -> Just <$> KC.Val w x <$> (kcType w t *> pure t)
  TR.TopLet w ds   -> pure $ Just $ KC.TopLet w (fmap retagDefn ds)
  TR.TopRec w ds   -> pure $ Just $ KC.TopRec w (fmap retagDefn ds)
  TR.Asm    w x  a -> pure $ Just $ KC.Asm w x a

kcModule ::TR.Module -> KC KC.Module
kcModule module_ = catMaybes <$> traverse kcTopLevel module_

checkModule :: MonadError String m => TR.Module -> m KC.Module
checkModule = runExcept . kcModule
