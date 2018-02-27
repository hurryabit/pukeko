module Pukeko.AST.Unwind
  ( unwindl
  , unwindr
  , rewindr
  ) where

import Pukeko.Prelude

import Control.Lens

-- | Unwind the spine of a left associative operation like application.
unwindl :: Prism' e (e, a) -> e -> (e, [a])
unwindl p = go []
  where
    go args expr = case matching p expr of
      Right (fun, arg) -> go (arg:args) fun
      Left   fun       -> (fun, args)

-- | Unwind the spine of a right associative operation like abstraction.
unwindr :: Prism' e (x, e) -> e -> ([x], e)
unwindr p = go []
  where
    go params expr = case matching p expr of
      Right (param, body) -> go (param:params) body
      Left          body  -> (reverse params, body)

rewindr :: Prism' e (x, e) -> [x] -> e -> e
rewindr p params body = foldr (\param expr -> p # (param, expr)) body params
