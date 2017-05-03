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

expr :: Applicative f
     => (Expr con a -> f (Expr con a)) -> Expr con a -> f (Expr con a)
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

defn :: Applicative f
     => (Expr con a -> f (Expr con a)) -> Defn con a -> f (Defn con a)
defn f d@MkDefn{_rhs} = do
  _rhs <- f _rhs
  return (d{_rhs} :: Defn _ _)

altn :: Applicative f
     => (Expr con a -> f (Expr con a)) -> Altn con a -> f (Altn con a)
altn f a@MkAltn{_rhs} = do
  _rhs <- f _rhs
  pure (a{_rhs} :: Altn _ _)

topLevel :: Applicative f
         => (Expr con a -> f (Expr con a)) -> TopLevel con a -> f (TopLevel con a)
topLevel f t = case t of
  Def{_defns} -> do
    _defns <- traverse (defn f) _defns
    pure (t{_defns} :: TopLevel _ _)
  _ -> pure t

module_ :: Applicative f
        => (Expr con a -> f (Expr con a)) -> Module con a -> f (Module con a)
module_ f = traverse (topLevel f)
