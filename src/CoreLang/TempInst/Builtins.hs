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


withDataArg :: (Node -> TIM ()) -> TIM ()
withDataArg cont = do
  pop
  appAddr <- top
  Application _ argAddr <- deref appAddr
  argNode <- deref argAddr
  if isDataNode argNode then do
    cont argNode
  else do
    dump
    push argAddr

updateTopNode :: Node -> TIM ()
updateTopNode node = do
  addr <- top
  update addr node

prim1 :: (Node -> TIM Node) -> TIM ()
prim1 impl =
  withDataArg $ \argNode ->
    impl argNode >>= updateTopNode

arith1 :: (Integer -> Integer) -> TIM ()
arith1 op = prim1 $ \node -> mkNumber <$> op <$> getNumber node


prim2 :: (Node -> Node -> TIM Node) -> TIM ()
prim2 impl = 
  withDataArg $ \arg1Node -> do
    withDataArg $ \arg2Node -> do
      impl arg1Node arg2Node >>= updateTopNode

arith2 :: (Integer -> Integer -> Integer) -> TIM ()
arith2 op = prim2 $ \node1 node2 ->
  mkNumber <$> (op <$> getNumber node1 <*> getNumber node2)

rel2 :: (Integer -> Integer -> Bool) -> TIM ()
rel2 cmp = prim2 $ \node1 node2 ->
  mkBool <$> (cmp <$> getNumber node1 <*> getNumber node2)


if_ :: TIM ()
if_ =
  withDataArg $ \condNode -> do
    pop
    thenAppAddr <- top
    pop
    elseAppAddr <- top
    cond <- getBool condNode
    if cond then do
      Application _ thenAddr <- deref thenAppAddr
      updateTopNode (Indirection thenAddr)
    else do
      Application _ elseAddr <- deref elseAppAddr
      updateTopNode (Indirection elseAddr)

casePair :: TIM ()
casePair =
  withDataArg $ \pairNode -> do
    pop
    funAppAddr <- top
    case pairNode of
      Data 0 [arg1Addr, arg2Addr] -> do
        Application _ funAddr <- deref funAppAddr
        arg1AppAddr <- alloc (Application funAddr arg1Addr)
        updateTopNode (Application arg1AppAddr arg2Addr)
      _ -> throwError "pattern match failed on case_pair"

caseList :: TIM ()
caseList =
  withDataArg $ \listNode -> do
    pop
    baseAppAddr <- top
    pop
    stepAppAddr <- top
    Data t argAddrs <- return listNode
    case (t, argAddrs) of
      (0, []) -> do
        Application _ baseAddr <- deref baseAppAddr
        updateTopNode (Indirection baseAddr)
      (1, [arg1Addr, arg2Addr]) -> do
        Application _ stepAddr <- deref stepAppAddr
        arg1AppAddr <- alloc (Application stepAddr arg1Addr)
        updateTopNode (Application arg1AppAddr arg2Addr)
      _ -> throwError "pattern match failed in case_list"


printNum :: TIM ()
printNum = do
  no_dump <- dumpNull
  if no_dump then do
    withDataArg $ \outNode -> do
      pop
      num <- getNumber outNode
      output (show num)
      retAppAddr <- top
      Application _ retAddr <- deref retAppAddr
      updateTopNode (Indirection retAddr)
  else
    throwError "dump not empty"


abort :: TIM ()
abort = throwError "user aborted execution"
