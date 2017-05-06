{-# LANGUAGE ApplicativeDo #-}
module Pukeko.Language.Rewrite
  ( expr
  , defn
  , altn
  , topLevel
  , module_
  )
where

import Pukeko.Language.Syntax

expr :: Applicative f => (Expr a -> f (Expr a)) -> Expr a -> f (Expr a)
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
  Match{_expr, _altns} -> do
    _expr <- f _expr
    _altns <- traverse (altn f) _altns
    pure e{_expr, _altns}

defn :: Applicative f => (Expr a -> f (Expr a)) -> Defn a -> f (Defn a)
defn f d@MkDefn{_rhs} = do
  _rhs <- f _rhs
  return (d{_rhs} :: Defn _)

altn :: Applicative f => (Expr a -> f (Expr a)) -> Altn a -> f (Altn a)
altn f a@MkAltn{_rhs} = do
  _rhs <- f _rhs
  pure (a{_rhs} :: Altn _)

topLevel :: Applicative f => (Expr a -> f (Expr a)) -> TopLevel a -> f (TopLevel a)
topLevel f t = case t of
  Def{_defns} -> do
    _defns <- traverse (defn f) _defns
    pure (t{_defns} :: TopLevel _)
  _ -> pure t

module_ :: Applicative f => (Expr a -> f (Expr a)) -> Module a -> f (Module a)
module_ f = traverse (topLevel f)
