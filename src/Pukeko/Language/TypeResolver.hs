module Pukeko.Language.TypeResolver
  ( Module
  , resolveModule
  )
where

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable       (for_)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (isJust)
import           Data.Traversable    (for)

import           Pukeko.Error
import           Pukeko.Language.Base.AST
import           Pukeko.Language.TypeResolver.AST
import qualified Pukeko.Language.DeBruijner.AST   as D
import qualified Pukeko.Language.Type             as Ty
import qualified Pukeko.Language.Ident            as Id

data TRState = MkTRState
  { _typeCons :: Map Id.Con TypeCon
  , _exprCons :: Map Id.Con ExprCon
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
trType :: Pos -> Ty.Type Id.Con Ty.Closed -> TR (Ty.Type TypeCon Ty.Closed)
trType posn typ = case typ of
    Ty.Var var -> return $ Ty.Var var
    Ty.Fun tx ty -> Ty.Fun <$> trType posn tx <*> trType posn ty
    Ty.App con typs -> do
      adt_opt <- Map.lookup con <$> use typeCons
      case adt_opt of
        Nothing -> throwAt posn "unknown type cons" con
        Just adt -> Ty.App adt <$> traverse (trType posn) typs

-- TODO: Have only one insert function.
insertTypeCon :: Pos -> TypeCon -> TR ()
insertTypeCon posn adt@Ty.MkADT{_name} = do
  old <- use (typeCons . at _name)
  when (isJust old) $ throwAt posn "duplicate type cons" _name
  typeCons . at _name ?= adt

insertExprCon :: Pos -> ExprCon -> TR ()
insertExprCon posn con@Ty.MkConstructor{_name} = do
  old <- use (exprCons . at _name)
  when (isJust old) $ throwAt posn "duplicate term cons" _name
  exprCons . at _name ?= con

findExprCon :: Pos -> Id.Con -> TR ExprCon
findExprCon posn name = do
  con_opt <- Map.lookup name <$> use exprCons
  case con_opt of
    Nothing -> throwAt posn "unknown term cons" name
    Just con -> return con

trTopLevel :: D.TopLevel -> TR TopLevel
trTopLevel top = case top of
  D.TypDef w adts -> do
    for_ adts (insertTypeCon w)
    adts <- for adts $ \_adt@Ty.MkADT{_constructors} -> do
      _constructors <- forM _constructors $ \con@Ty.MkConstructor{_fields} -> do
        _fields <- traverse (trType w) _fields
        let con' = con{Ty._adt, Ty._fields}
        insertExprCon w con'
        return con'
      return _adt{Ty._constructors}
    return (TypDef w adts)
  D.Val w x t -> Val w x <$> trType w t
  D.TopLet w ds -> TopLet w <$> itraverseOf (traverse . defnExprCons) findExprCon ds
  D.TopRec w ds -> TopRec w <$> itraverseOf (traverse . defnExprCons) findExprCon ds
  D.Asm w x a -> pure $ Asm w x a

resolveModule :: MonadError String m => D.Module -> m Module
resolveModule = runTR . traverse trTopLevel


-- * Optics

defnExprCons :: IndexedTraversal Pos (D.Defn v) (Defn v) Id.Con ExprCon
defnExprCons = rhs2 . exprExprCons

exprExprCons :: IndexedTraversal Pos (D.Expr v) (Expr v) Id.Con ExprCon
exprExprCons f = \case
  D.Var w x       -> pure $ Var w x
  D.Con w c       -> Con w <$> indexed f w c
  D.Num w n       -> pure $ Num w n
  D.App w t  us   -> App w <$> exprExprCons f t <*> (traverse . exprExprCons) f us
  -- D.If  w t  u  v -> If  w <$> exprExprCons f t <*> exprExprCons f u <*> exprExprCons f v
  D.Mat w ts as   -> Mat w <$> exprExprCons f ts <*> (traverse . altnExprCons) f as
  D.Lam w bs t    -> Lam w bs <$> exprExprCons f t
  D.Let w ds t    -> Let w <$> (traverse . defnExprCons) f ds <*> exprExprCons f t
  D.Rec w ds t    -> Rec w <$> (traverse . defnExprCons) f ds <*> exprExprCons f t

altnExprCons :: IndexedTraversal Pos (D.Altn v) (Altn v) Id.Con ExprCon
altnExprCons f (MkAltn w c bs t) =
  MkAltn w <$> indexed f w c <*> pure bs <*> exprExprCons f t
