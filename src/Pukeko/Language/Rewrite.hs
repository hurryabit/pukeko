module Pukeko.Language.Rewrite
  ( bottomUp
  , bottomUpM
  , topDown
  , topDownM
  , Rewrite (rewrite_expr, rewrite_defn, rewrite_bind, rewrite_altn)
  , emptyRewrite
  , runRewrite
  )
  where

import Control.Monad.Identity

import Pukeko.Language.Syntax

bottomUp :: (Expr a -> Expr a)
         -> (Defn a -> Defn a)
         -> (Bind a -> Bind a)
         -> (Altn a -> Altn a)
         -> (Expr a -> Expr a)
bottomUp post_expr post_defn post_bind post_altn =
  runIdentity . bottomUpM
  (Identity . post_expr)
  (Identity . post_defn)
  (Identity . post_bind)
  (Identity . post_altn)

bottomUpM :: Monad m => (Expr a -> m (Expr a))
                     -> (Defn a -> m (Defn a))
                     -> (Bind a -> m (Bind a))
                     -> (Altn a -> m (Altn a))
                     -> (Expr a -> m (Expr a))
bottomUpM post_expr post_defn post_bind post_altn =
  runRewrite $ emptyRewrite
    { rewrite_expr = \descend expr -> descend expr >>= post_expr
    , rewrite_defn = \descend defn -> descend defn >>= post_defn
    , rewrite_bind = \descend bind -> descend bind >>= post_bind
    , rewrite_altn = \descend altn -> descend altn >>= post_altn
    }

topDown :: (Expr a -> Expr a)
        -> (Defn a -> Defn a)
        -> (Bind a -> Bind a)
        -> (Altn a -> Altn a)
        -> (Expr a -> Expr a)
topDown pre_expr pre_defn pre_bind pre_altn =
  runIdentity . topDownM
  (Identity . pre_expr)
  (Identity . pre_defn)
  (Identity . pre_bind)
  (Identity . pre_altn)

topDownM :: Monad m => (Expr a -> m (Expr a))
                    -> (Defn a -> m (Defn a))
                    -> (Bind a -> m (Bind a))
                    -> (Altn a -> m (Altn a))
                    -> (Expr a -> m (Expr a))
topDownM pre_expr pre_defn pre_bind pre_altn =
  runRewrite $ emptyRewrite
    { rewrite_expr = \descend expr -> pre_expr expr >>= descend
    , rewrite_defn = \descend defn -> pre_defn defn >>= descend
    , rewrite_bind = \descend bind -> pre_bind bind >>= descend
    , rewrite_altn = \descend altn -> pre_altn altn >>= descend
    }

data Rewrite m a = MkRewrite
  { rewrite_expr :: (Expr a -> m (Expr a)) -> Expr a -> m (Expr a)
  , rewrite_defn :: (Defn a -> m (Defn a)) -> Defn a -> m (Defn a)
  , rewrite_bind :: (Bind a -> m (Bind a)) -> Bind a -> m (Bind a)
  , rewrite_altn :: (Altn a -> m (Altn a)) -> Altn a -> m (Altn a)
  }

emptyRewrite :: Rewrite m a
emptyRewrite = MkRewrite
  { rewrite_expr = ($)
  , rewrite_defn = ($)
  , rewrite_bind = ($)
  , rewrite_altn = ($)
  }

runRewrite :: Monad m => Rewrite m a -> Expr a -> m (Expr a)
runRewrite MkRewrite{ rewrite_expr, rewrite_defn, rewrite_bind, rewrite_altn } =
  whole_expr
  where
    whole_expr = rewrite_expr sub_expr
    whole_defn = rewrite_defn sub_defn
    whole_bind = rewrite_bind sub_bind
    whole_altn = rewrite_altn sub_altn
    sub_expr expr = do
      case expr of
        Var { }  -> return expr
        Num { }  -> return expr
        Ap { _fun, _args } -> do
          _fun  <- whole_expr _fun
          _args <- mapM whole_expr _args
          return $ expr { _fun, _args }
        ApOp { _arg1, _arg2 } -> do
          _arg1 <- whole_expr _arg1
          _arg2 <- whole_expr _arg2
          return $ expr { _arg1, _arg2 }
        Let { _defns, _body } -> do
          _defns <- mapM whole_defn _defns
          _body  <- whole_expr _body
          return $ expr { _defns, _body }
        Lam { _binds, _body } -> do
          _binds <- mapM whole_bind _binds
          _body  <- whole_expr _body
          return $ expr { _binds, _body }
        If { _cond, _then, _else } -> do
          _cond <- whole_expr _cond
          _then <- whole_expr _then
          _else <- whole_expr _else
          return $ expr { _cond, _then, _else }
        Match { _expr, _altns } -> do
          _expr  <- whole_expr _expr
          _altns <- mapM whole_altn _altns
          return $ expr { _expr, _altns }
    sub_defn defn@MkDefn{ _bind , _expr } = do
      _bind <- whole_bind _bind
      _expr <- whole_expr _expr
      return $ defn { _bind, _expr }
    sub_bind bind =
      return $ bind
    sub_altn altn@MkAltn{ _binds, _rhs } = do
      _binds <- mapM whole_bind _binds
      _rhs   <- whole_expr _rhs
      return $ altn { _binds, _rhs }
