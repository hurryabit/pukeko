{-# LANGUAGE TypeOperators #-}
module Pukeko.Language.LambdaLifter
  ( Out
  , liftModule
  )
where

import           Control.Lens
import           Control.Lens.Extras (is)
import           Control.Monad.RWS
import qualified Data.Finite       as Fin
import           Data.Foldable     (traverse_)
import           Data.List         (sortOn)
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Set.Lens     as Set
import qualified Data.Vector.Sized as Vec

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage   as St
import qualified Pukeko.Language.Ident       as Id
import           Pukeko.Language.Type        (NoType (..))

type In  = St.TypeEraser
type Out = St.LambdaLifter

type State = [Id.EVar]

data Env v = MkEnv
  { _mkGlobal :: Id.EVar -> v
  , _isGlobal :: v -> Bool
  }

newtype LL v a = LL{unLL :: RWS (Env v) [TopLevel Out] State a}
  deriving ( Functor, Applicative, Monad
           , MonadReader (Env v)
           , MonadWriter [TopLevel Out]
           , MonadState State
           )

execLL :: LL Id.EVar () -> [TopLevel Out]
execLL ll =
  let env = MkEnv id (const True)
      state = []
      ((), defns) = evalRWS (unLL ll) env state
  in  defns

freshIdent :: LL v Id.EVar
freshIdent = state $ \(ident:idents) -> (ident, idents)

scoped :: LL (EScope i v) a -> LL v a
scoped =
  LL . withRWST(\(MkEnv mk is) s -> (MkEnv (Free . mk) (scope (const False) is), s)) . unLL

llExpr :: (IsEVar v, Ord v) => Expr In Void v -> LL v (Expr Out Void v)
llExpr = \case
  EVar w x -> pure (EVar w x)
  ECon w c -> pure (ECon w c)
  ENum w n -> pure (ENum w n)
  EApp w t  us -> EApp w <$> llExpr t <*> traverse llExpr us
  ECas w t  cs -> ECas w <$> llExpr t <*> traverse llCase cs
  ELet w ds t -> ELet w <$> (traverse . defn2rhs) llExpr ds <*> scoped (llExpr t)
  ERec w ds t -> scoped $ ERec w <$> (traverse . defn2rhs) llExpr ds <*> llExpr t
  ELam w oldBinds rhs -> do
    lhs <- freshIdent
    (rhs, isGlobal) <- scoped ((,) <$> llExpr rhs <*> asks _isGlobal)
    let isCaptured v = is _Free v && not (isGlobal v)
    let (capturedS, others) = Set.partition isCaptured $ Set.setOf traverse rhs
    -- TODO: Arrange 'captured' in a clever way. The @sortOn varName@ is just to
    -- not break the tests right now.
    let capturedL = sortOn baseName $ Set.toList capturedS
    Vec.withList capturedL $ \(capturedV :: Vec.Vector m (EFinScope n v)) -> do
      let newBinds = fmap (\x -> MkBind w (baseName x) NoType) capturedV Vec.++ oldBinds
      let renameOther = \case
            Bound i x -> Bound (Fin.shift i) x
            Free  v   -> Free  (baseName v)
      let renameCaptured i v = Map.singleton v (mkBound (Fin.weaken i) (baseName v))
      let rename = Map.fromSet renameOther others <> ifoldMap renameCaptured capturedV
      tell [TLSup (MkBind w lhs NoType) (fmap retagBind newBinds) (fmap (rename Map.!) rhs)]
      let unfree = \case
            Bound{} -> undefined -- NOTE: Everyhing in @capturedL@ starts with 'Free'.
            Free v  -> v
      mkGlobal <- asks _mkGlobal
      let fun = EVar w (mkGlobal lhs)
      case capturedL of
        []  -> return fun
        _:_ -> return $ EApp w fun (map (EVar w . unfree) capturedL)

llCase :: (IsEVar v, Ord v) => Case In Void v -> LL v (Case Out Void v)
llCase (MkCase w c bs e) = MkCase w c bs <$> scoped (llExpr e)

llTopLevel :: TopLevel In -> LL Id.EVar ()
llTopLevel = \case
  TLDef (MkBind w lhs _) rhs -> do
    put $ Id.freshEVars "ll" lhs
    case rhs of
      ELam{} -> do
        -- NOTE: Make sure this lambda gets lifted to the name it is defining.
        modify (lhs:)
        void $ llExpr rhs
      _ -> do
        rhs <- llExpr rhs
        tell [TLCaf (MkBind w lhs NoType) rhs]
  TLAsm b asm -> tell [TLAsm (retagBind b) asm]

liftModule :: Module In -> Module Out
liftModule = over module2tops (execLL . traverse_ llTopLevel)
