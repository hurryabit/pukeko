module CoreLang.TempInst.Instantiate
  ( instantiateWithAlloc
  , instantiateWithUpdate
  )
  where

import CoreLang.Language.Syntax
import CoreLang.TempInst.TIM

instantiateWithAlloc :: Expr -> TIM Addr
instantiateWithAlloc body =
  case body of
    Num n -> alloc (Number n)
    Var x -> resolve x
    Ap expr1 expr2 -> do
      addr1 <- instantiateWithAlloc expr1
      addr2 <- instantiateWithAlloc expr2
      alloc (Application addr1 addr2)
    Let NonRecursive defs expr -> do
      bindings <-
        forM defs $ \(x, rhs) -> do
          addr <- instantiateWithAlloc rhs
          return (x, addr)
      local (bindings ++) (instantiateWithAlloc expr)
    Let Recursive defs expr -> do
      bindings <-
        forM defs $ \(x, _) -> do
          addr <- alloc (Number 0)
          return (x, addr)
      local (bindings ++) $ do
        forM_ (zip defs bindings) $ \((_, rhs), (_, addr)) -> do
          instantiateWithUpdate rhs addr
        instantiateWithAlloc expr
    Pack t n -> alloc (Constructor t n)
    Case _ _ -> throwError "case statements not implemented"
    Lam  _ _ -> throwError "lambdas not implemented"

instantiateWithUpdate :: Expr -> Addr -> TIM ()
instantiateWithUpdate body target =
  case body of
    Num n -> update target (Number n)
    Var x -> do
      addr <- resolve x
      update target (Indirection addr)
    Ap expr1 expr2 -> do
      addr1 <- instantiateWithAlloc expr1
      addr2 <- instantiateWithAlloc expr2
      update target (Application addr1 addr2)
    Let NonRecursive defs expr -> do
      bindings <-
        forM defs $ \(x, rhs) -> do
          addr <- instantiateWithAlloc rhs
          return (x, addr)
      local (bindings ++) (instantiateWithUpdate expr target)
    Let Recursive defs expr -> do
      bindings <-
        forM defs $ \(x, _) -> do
          addr <- alloc (Number 0)
          return (x, addr)
      local (bindings ++) $ do
        forM_ (zip defs bindings) $ \((_, rhs), (_, addr)) -> do
          instantiateWithUpdate rhs addr
        instantiateWithUpdate expr target
    Pack t n -> update target (Constructor t n)
    Case _ _ -> throwError "case statements not implemented"
    Lam  _ _ -> throwError "lambdas not implemented"
