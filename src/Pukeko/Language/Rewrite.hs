{-# LANGUAGE ApplicativeDo #-}
module Pukeko.Language.Rewrite
  ( type_
  , expr
  , defn
  , altn
  , topLevel
  , module_
  )
where

import Pukeko.Language.Syntax
import Pukeko.Language.Type (Type (..))

type_ :: Applicative f => (Type con a -> f (Type con a)) -> Type con a -> f (Type con a)
type_ f t = case t of
  TVar{} -> pure t
  TFun tx ty -> TFun <$> type_ f tx <*> type_ f ty
  TApp c ts -> TApp c <$> traverse (type_ f) ts
  UVar{} -> pure t

expr :: Applicative f => (Expr stage a -> f (Expr stage a)) -> Expr stage a -> f (Expr stage a)
expr f e = case e of
  Var{} -> pure e
  Con{} -> pure e
  Num{} -> pure e
  Ap{_fun, _args} -> do
    _fun <- f _fun
    _args <- traverse f _args
    pure e{_fun, _args}
  Lam{_body} -> do
    _body <- f _body
    pure e{_body}
  Let{_defns, _body} -> do
    _defns <- traverse (defn f) _defns
    _body <- f _body
    pure e{_defns, _body}
  If{_cond, _then, _else} -> do
    _cond <- f _cond
    _then <- f _then
    _else <- f _else
    pure e{_cond, _then, _else}
  Match{_exprs, _altns} -> do
    _exprs <- traverse f _exprs
    _altns <- traverse (altn f) _altns
    pure e{_exprs, _altns}

defn :: Applicative f => (Expr stage a -> f (Expr stage a)) -> Defn stage a -> f (Defn stage a)
defn f d@MkDefn{_rhs} = do
  _rhs <- f _rhs
  return (d{_rhs} :: Defn _ _)

altn :: Applicative f => (Expr stage a -> f (Expr stage a)) -> Altn stage a -> f (Altn stage a)
altn f a@MkAltn{_rhs} = do
  _rhs <- f _rhs
  pure (a{_rhs} :: Altn _ _)

topLevel :: Applicative f => (Expr stage a -> f (Expr stage a)) -> TopLevel stage a -> f (TopLevel stage a)
topLevel f t = case t of
  Def{_defns} -> do
    _defns <- traverse (defn f) _defns
    pure (t{_defns} :: TopLevel _ _)
  _ -> pure t

module_ :: Applicative f => (Expr stage a -> f (Expr stage a)) -> Module stage a -> f (Module stage a)
module_ f = traverse (topLevel f)
