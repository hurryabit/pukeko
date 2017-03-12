module Pukeko.Language.Rewrite
  ( bottomUp
  , bottomUpM
  , topDown
  , topDownM
  , Rewrite (rewrite_expr, rewrite_defn, rewrite_patn, rewrite_altn)
  , emptyRewrite
  , runRewrite
  )
  where

import Control.Monad.Identity

import Pukeko.Language.Syntax

bottomUp :: (Expr a -> Expr a)
         -> (Defn a -> Defn a)
         -> (Patn a -> Patn a)
         -> (Altn a -> Altn a)
         -> (Expr a -> Expr a)
bottomUp post_expr post_defn post_patn post_altn =
  runIdentity . bottomUpM
  (Identity . post_expr)
  (Identity . post_defn)
  (Identity . post_patn)
  (Identity . post_altn)

bottomUpM :: Monad m => (Expr a -> m (Expr a))
                     -> (Defn a -> m (Defn a))
                     -> (Patn a -> m (Patn a))
                     -> (Altn a -> m (Altn a))
                     -> (Expr a -> m (Expr a))
bottomUpM post_expr post_defn post_patn post_altn =
  runRewrite $ emptyRewrite
    { rewrite_expr = \descend expr -> descend expr >>= post_expr
    , rewrite_defn = \descend defn -> descend defn >>= post_defn
    , rewrite_patn = \descend patn -> descend patn >>= post_patn
    , rewrite_altn = \descend altn -> descend altn >>= post_altn
    }

topDown :: (Expr a -> Expr a)
        -> (Defn a -> Defn a)
        -> (Patn a -> Patn a)
        -> (Altn a -> Altn a)
        -> (Expr a -> Expr a)
topDown pre_expr pre_defn pre_patn pre_altn =
  runIdentity . topDownM
  (Identity . pre_expr)
  (Identity . pre_defn)
  (Identity . pre_patn)
  (Identity . pre_altn)

topDownM :: Monad m => (Expr a -> m (Expr a))
                    -> (Defn a -> m (Defn a))
                    -> (Patn a -> m (Patn a))
                    -> (Altn a -> m (Altn a))
                    -> (Expr a -> m (Expr a))
topDownM pre_expr pre_defn pre_patn pre_altn =
  runRewrite $ emptyRewrite
    { rewrite_expr = \descend expr -> pre_expr expr >>= descend
    , rewrite_defn = \descend defn -> pre_defn defn >>= descend
    , rewrite_patn = \descend patn -> pre_patn patn >>= descend
    , rewrite_altn = \descend altn -> pre_altn altn >>= descend
    }

data Rewrite m a = MkRewrite
  { rewrite_expr :: (Expr a -> m (Expr a)) -> Expr a -> m (Expr a)
  , rewrite_defn :: (Defn a -> m (Defn a)) -> Defn a -> m (Defn a)
  , rewrite_patn :: (Patn a -> m (Patn a)) -> Patn a -> m (Patn a)
  , rewrite_altn :: (Altn a -> m (Altn a)) -> Altn a -> m (Altn a)
  }

emptyRewrite :: Rewrite m a
emptyRewrite = MkRewrite
  { rewrite_expr = ($)
  , rewrite_defn = ($)
  , rewrite_patn = ($)
  , rewrite_altn = ($)
  }

runRewrite :: Monad m => Rewrite m a -> Expr a -> m (Expr a)
runRewrite MkRewrite{ rewrite_expr, rewrite_defn, rewrite_patn, rewrite_altn } =
  whole_expr
  where
    whole_expr = rewrite_expr sub_expr
    whole_defn = rewrite_defn sub_defn
    whole_patn = rewrite_patn sub_patn
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
        Lam { _patns, _body } -> do
          _patns <- mapM whole_patn _patns
          _body  <- whole_expr _body
          return $ expr { _patns, _body }
        If { _cond, _then, _else } -> do
          _cond <- whole_expr _cond
          _then <- whole_expr _then
          _else <- whole_expr _else
          return $ expr { _cond, _then, _else }
        Match { _expr, _altns } -> do
          _expr  <- whole_expr _expr
          _altns <- mapM whole_altn _altns
          return $ expr { _expr, _altns }
    sub_defn defn@MkDefn{ _patn , _expr } = do
      _patn <- whole_patn _patn
      _expr <- whole_expr _expr
      return $ defn { _patn, _expr }
    sub_patn patn =
      return $ patn
    sub_altn altn@MkAltn{ _patns, _rhs } = do
      _patns <- mapM whole_patn _patns
      _rhs   <- whole_expr _rhs
      return $ altn { _patns, _rhs }
