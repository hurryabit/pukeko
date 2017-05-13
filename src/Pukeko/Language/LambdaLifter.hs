module Pukeko.Language.LambdaLifter
  ( liftModule
  )
where

import Control.Monad.RWS
import Data.Set (Set)
import qualified Data.Set as Set

import Pukeko.Language.Syntax
import Pukeko.Language.Ident as Ident
import qualified Pukeko.Language.Rewrite as Rewrite

type State = [Ident.EVar]

type FV = Set Ident.EVar

newtype LL a = LL{unLL :: RWS () [TopLevel StageTR FV] State a}
  deriving ( Functor, Applicative, Monad
           , MonadWriter [TopLevel StageTR FV]
           , MonadState State
           )

execLL :: LL () -> Module StageTR FV
execLL ll =
  let state = []
      ((), defns) = evalRWS (unLL ll) () state
  in  defns

emit :: Defn StageTR FV -> LL ()
emit defn = tell [Def{_annot = Set.empty, _isrec = False, _defns = [defn]}]

freshIdent :: LL Ident.EVar
freshIdent = state $ \(ident:idents) -> (ident, idents)

llExpr :: Expr StageTR FV -> LL (Expr StageTR FV)
llExpr expr = case expr of
  Lam{_annot, _binds, _body} -> do
    _ident <- freshIdent
    _body <- llExpr _body
    -- TODO: Use a clever order here.
    let fvs = Set.toList _annot
        mkBind f ident =
          MkBind{_annot = Set.singleton ident, _ident = f ident}
        _rhs = Lam{_annot = Set.empty, _binds = map (mkBind Just) fvs ++ _binds, _body}
        _lhs = mkBind id _ident
    emit MkDefn{_lhs, _rhs}
    let mkGlobalVar _var = Var{_annot = Set.empty, _var}
        mkLocalVar _var = Var{_annot = Set.singleton _var, _var}
    return $ mkAp _annot (mkGlobalVar _ident) (map mkLocalVar fvs)
  _ -> Rewrite.expr llExpr expr

-- TODO: Fix the awful hack for the right naming of non-CAFs.
llTopDefn :: Defn StageTR FV -> LL ()
llTopDefn defn@MkDefn{_lhs = MkBind{_ident}, _rhs} = do
  let is_lambda = case _rhs of
        Lam{} -> True
        _     -> False
  put $ (if is_lambda then [_ident] else []) ++ Ident.freshEVars _ident
  defn <- Rewrite.defn llExpr defn
  unless is_lambda $ emit defn

llModule :: Module StageTR FV -> LL ()
llModule module_ = do
  forM_ module_ $ \top ->
    case top of
      Def{_defns} -> mapM_ llTopDefn _defns
      _ -> tell [top]

liftModule :: Module StageTR FV -> Module StageTR FV
liftModule = execLL . llModule
