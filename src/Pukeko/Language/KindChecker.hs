-- | Check that all type constructor are applied to the right number of
-- variables and all variables are bound.
module Pukeko.Language.KindChecker
  ( Module
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
import           Pukeko.Language.Base.AST
import qualified Pukeko.Language.TypeResolver.AST as R
import           Pukeko.Language.KindChecker.AST
import qualified Pukeko.Language.Type             as Ty

type Type = Ty.Type TypeCon Ty.Closed

type KC a = Except String a

kcType :: Pos -> Type -> KC ()
kcType w = itraverseOf_ Ty.typeCons $ \typs Ty.MkADT{_name, _params} -> do
  -- TODO: Check that params are mutually distinct.
  when (length _params /= length typs) $ throwDocAt w $
    "type cons" <+> quotes (pretty _name) <+> "expects" <+>
    int (length _params) <+> "parameters"

kcTopLevel :: R.TopLevel -> KC (Maybe TopLevel)
kcTopLevel = \case
  R.TypDef w adts -> do
    for_ adts $ \Ty.MkADT{_params, _constructors} -> do
      for_ _constructors $ \Ty.MkConstructor{_name, _fields} -> do
        let unbound =
              Set.unions (map Ty.vars _fields) `Set.difference` Set.fromList _params
        unless (Set.null unbound) $ throwAt w "unbound type vars in term cons" _name
        traverse_ (kcType w) _fields
    return Nothing
  R.Val    w x  t -> Just <$> Val w x <$> (kcType w t *> pure t)
  R.TopLet w ds   -> pure $ Just $ TopLet w ds
  R.TopRec w ds   -> pure $ Just $ TopRec w ds
  R.Asm    w x  a -> pure $ Just $ Asm w x a

kcModule ::R.Module -> KC Module
kcModule module_ = catMaybes <$> traverse kcTopLevel module_

checkModule :: MonadError String m => R.Module -> m Module
checkModule = runExcept . kcModule
