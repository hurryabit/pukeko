module CoreLang.TempInst.Builtins
  ( everything
  )
  where

import CoreLang.Language.Syntax (Identifier)
import CoreLang.TempInst.TIM

everything :: [(Identifier, TIM ())]
everything =
  [ ("neg", arith1 negate)
  , ("+"  , arith2 (+))
  , ("-"  , arith2 (-))
  , ("*"  , arith2 (*))
  , ("/"  , arith2 div)
  , ("<"  , rel2 (<) )
  , ("<=" , rel2 (<=))
  , ("==" , rel2 (==))
  , ("!=" , rel2 (/=))
  , (">=" , rel2 (>=))
  , (">"  , rel2 (>) )
  , ("if"       , if_     )
  , ("case_pair", casePair)
  , ("case_list", caseList)
  , ("print"    , printNum)
  , ("abort"    , abort   )
  ]


prim1 :: (Node -> TIM Node) -> TIM ()
prim1 impl = do
  pop
  appAddr <- top
  Application _ argAddr <- deref appAddr
  argNode <- deref argAddr
  if isDataNode argNode then do
    resNode <- impl argNode
    update appAddr resNode
  else do
    dump
    push argAddr

arith1 :: (Integer -> Integer) -> TIM ()
arith1 op = prim1 $ \node -> mkNumber <$> op <$> getNumber node


prim2 :: (Node -> Node -> TIM Node) -> TIM ()
prim2 impl = do
  pop
  app1Addr <- top
  Application _ arg1Addr <- deref app1Addr
  arg1Node <- deref arg1Addr
  if isDataNode arg1Node then do
    pop
    app2Addr <- top
    Application _ arg2Addr <- deref app2Addr
    arg2Node <- deref arg2Addr
    if isDataNode arg2Node then do
      resNode <- impl arg1Node arg2Node
      update app2Addr resNode
    else do
      dump
      push arg2Addr
  else do
    dump
    push arg1Addr

arith2 :: (Integer -> Integer -> Integer) -> TIM ()
arith2 op = prim2 $ \node1 node2 ->
  mkNumber <$> (op <$> getNumber node1 <*> getNumber node2)

rel2 :: (Integer -> Integer -> Bool) -> TIM ()
rel2 cmp = prim2 $ \node1 node2 ->
  mkBool <$> (cmp <$> getNumber node1 <*> getNumber node2)


if_ :: TIM ()
if_ = do
  pop
  condAppAddr <- top
  Application _ condAddr <- deref condAppAddr
  condNode <- deref condAddr
  if isDataNode condNode then do
    pop -- condAppAddr
    thenAppAddr <- top
    pop
    elseAppAddr <- top
    let rootAddr = elseAppAddr
    cond <- getBool condNode
    if cond then do
      Application _ thenAddr <- deref thenAppAddr
      update rootAddr (Indirection thenAddr)
    else do
      Application _ elseAddr <- deref elseAppAddr
      update rootAddr (Indirection elseAddr)
  else do
    dump
    push condAddr

casePair :: TIM ()
casePair = do
  pop
  pairAppAddr <- top
  Application _ pairAddr <- deref pairAppAddr
  pairNode <- deref pairAddr
  if isDataNode pairNode then do
    pop -- pairAppAddr
    funAppAddr <- top -- rootAddr
    Data 0 [arg1Addr, arg2Addr] <- return pairNode
    Application _ funAddr <- deref funAppAddr
    arg1AppAddr <- alloc (Application funAddr arg1Addr)
    update funAppAddr (Application arg1AppAddr arg2Addr)
  else do
    dump
    push pairAddr

caseList :: TIM ()
caseList = do
  pop
  listAppAddr <- top
  Application _ listAddr <- deref listAppAddr
  listNode <- deref listAddr
  if isDataNode listNode then do
    pop -- listAppAddr
    baseAppAddr <- top
    pop
    stepAppAddr <- top -- rootAddr
    Data t argAddrs <- return listNode
    case (t, argAddrs) of
      (0, []) -> do
        Application _ baseAddr <- deref baseAppAddr
        update stepAppAddr (Indirection baseAddr)
      (1, [arg1Addr, arg2Addr]) -> do
        Application _ stepAddr <- deref stepAppAddr
        arg1AppAddr <- alloc (Application stepAddr arg1Addr)
        update stepAppAddr (Application arg1AppAddr arg2Addr)
      _ -> throwError "pattern match failed in stepUnlist"
  else do
    dump
    push listAddr


printNum :: TIM ()
printNum = do
  no_dump <- dumpNull
  if no_dump then do
    pop
    outAppAddr <- top
    Application _ outAddr <- deref outAppAddr
    outNode <- deref outAddr
    if isDataNode outNode then do
      num <- getNumber outNode
      output (show num)
      pop -- outAppAddr
      retAppAddr <- top -- rootAddr
      Application _ retAddr <- deref retAppAddr
      update retAppAddr (Indirection retAddr)
    else do
      dump
      push outAddr
  else
    throwError "dump not empty"


abort :: TIM ()
abort = throwError "user aborted execution"
