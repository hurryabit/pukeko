module CoreLang.Language.Rewrite
  ( bottomUp
  , bottomUpM
  , topDown
  , topDownM
  , Rewrite (rewrite_expr, rewrite_defn, rewrite_patn)
  , emptyRewrite
  , runRewrite
  )
  where

import Control.Monad.Identity

import CoreLang.Language.Syntax

bottomUp :: (Expr a -> Expr a)
         -> (Defn a -> Defn a)
         -> (Patn a -> Patn a)
         -> (Expr a -> Expr a)
bottomUp post_expr post_defn post_patn =
  runIdentity . 
    bottomUpM (Identity . post_expr) (Identity . post_defn) (Identity . post_patn)

bottomUpM :: Monad m => (Expr a -> m (Expr a))
                     -> (Defn a -> m (Defn a))
                     -> (Patn a -> m (Patn a))
                     -> (Expr a -> m (Expr a))
bottomUpM post_expr post_defn post_patn =
  runRewrite $ emptyRewrite
    { rewrite_expr = \descend expr -> descend expr >>= post_expr
    , rewrite_defn = \descend defn -> descend defn >>= post_defn
    , rewrite_patn = \descend patn -> descend patn >>= post_patn
    }

topDown :: (Expr a -> Expr a)
        -> (Defn a -> Defn a)
        -> (Patn a -> Patn a)
        -> (Expr a -> Expr a)
topDown pre_expr pre_defn pre_patn =
  runIdentity . 
    bottomUpM (Identity . pre_expr) (Identity . pre_defn) (Identity . pre_patn)

topDownM :: Monad m => (Expr a -> m (Expr a))
                    -> (Defn a -> m (Defn a))
                    -> (Patn a -> m (Patn a))
                    -> (Expr a -> m (Expr a))
topDownM pre_expr pre_defn pre_patn =
  runRewrite $ emptyRewrite
    { rewrite_expr = \descend expr -> pre_expr expr >>= descend
    , rewrite_defn = \descend defn -> pre_defn defn >>= descend
    , rewrite_patn = \descend patn -> pre_patn patn >>= descend
    }

data Rewrite m a = MkRewrite
  { rewrite_expr :: (Expr a -> m (Expr a)) -> Expr a -> m (Expr a)
  , rewrite_defn :: (Defn a -> m (Defn a)) -> Defn a -> m (Defn a)
  , rewrite_patn :: (Patn a -> m (Patn a)) -> Patn a -> m (Patn a)
  }

emptyRewrite :: Rewrite m a
emptyRewrite = MkRewrite
  { rewrite_expr = ($)
  , rewrite_defn = ($)
  , rewrite_patn = ($)
  }

runRewrite :: Monad m => Rewrite m a -> Expr a -> m (Expr a)
runRewrite MkRewrite{ rewrite_expr, rewrite_defn, rewrite_patn } = whole_expr
  where
    whole_expr = rewrite_expr sub_expr
    whole_defn = rewrite_defn sub_defn
    whole_patn = rewrite_patn sub_patn
    sub_expr expr = do
      case expr of
        Var { }  -> return expr
        Num { }  -> return expr
        Pack { } -> return expr
        Ap { _fun, _arg } -> do
          _fun <- whole_expr _fun
          _arg <- whole_expr _arg
          return $ expr { _fun, _arg }
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
        Rec { _defns } -> do
          _defns <- mapM whole_defn _defns
          return $ expr { _defns }
        Sel { _expr } -> do
          _expr <- whole_expr _expr
          return $ (expr { _expr } :: Expr _)
    sub_defn defn@MkDefn{ _patn , _expr } = do
      _patn <- whole_patn _patn
      _expr <- whole_expr _expr
      return $ defn { _patn, _expr }
    sub_patn patn =
      return $ patn
