module Pukeko.Language.Optimizer where

import Pukeko.Language.Syntax

neutral :: GenExpr a -> GenExpr a
neutral = bottomUp $ \e ->
  case e of
    (Ap (Ap (Var "+") e1) (Num 0)) -> e1
    (Ap (Ap (Var "+") (Num 0)) e2) -> e2
    (Ap (Ap (Var "*") e1) (Num 1)) -> e1
    (Ap (Ap (Var "*") (Num 1)) e2) -> e2
    (Ap (Ap (Var "*") _) (Num 0))  -> Num 0
    (Ap (Ap (Var "*") (Num 0)) _)  -> Num 0
    _                              -> e

bottomUp :: (GenExpr a -> GenExpr a) -> GenExpr a -> GenExpr a
bottomUp f e = f $
  case e of
    Var x -> Var x
    Num n -> Num n
    Pack m n -> Pack m n
    Ap e1 e2 -> Ap (bottomUp f e1) (bottomUp f e2)
    Let r ds e1 ->
      let ds' = [ (x, bottomUp f e2) | (x, e2) <- ds ]
          e1' = bottomUp f e1
      in  Let r ds' e1'
    Case e1 as ->
      let e1' = bottomUp f e1
          as' = [ (n, xs, bottomUp f e2) | (n, xs, e2) <- as ]
      in  Case e1' as'
    Lam xs e1 -> Lam xs (bottomUp f e1)
