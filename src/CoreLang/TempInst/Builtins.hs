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


getArgAddr :: TIM Addr
getArgAddr = do
  pop
  appAddr <- top
  Application _ argAddr <- deref appAddr
  return argAddr

updateRoot :: Node -> TIM ()
updateRoot node = do
  addr <- top
  update addr node

withDataArg :: (Node -> TIM ()) -> TIM ()
withDataArg cont = do
  argAddr <- getArgAddr
  argNode <- deref argAddr
  if isDataNode argNode then do
    cont argNode
  else do
    dump
    push argAddr


prim1 :: (Node -> TIM Node) -> TIM ()
prim1 impl =
  withDataArg $ \argNode ->
    impl argNode >>= updateRoot

arith1 :: (Integer -> Integer) -> TIM ()
arith1 op = prim1 $ \node -> mkNumber <$> op <$> getNumber node


prim2 :: (Node -> Node -> TIM Node) -> TIM ()
prim2 impl = 
  withDataArg $ \arg1Node -> do
    withDataArg $ \arg2Node -> do
      impl arg1Node arg2Node >>= updateRoot

arith2 :: (Integer -> Integer -> Integer) -> TIM ()
arith2 op = prim2 $ \node1 node2 ->
  mkNumber <$> (op <$> getNumber node1 <*> getNumber node2)

rel2 :: (Integer -> Integer -> Bool) -> TIM ()
rel2 cmp = prim2 $ \node1 node2 ->
  mkBool <$> (cmp <$> getNumber node1 <*> getNumber node2)


if_ :: TIM ()
if_ =
  withDataArg $ \condNode -> do
    cond <- getBool condNode
    thenAddr <- getArgAddr
    elseAddr <- getArgAddr
    let rootAddr = if cond then thenAddr else elseAddr
    updateRoot (Indirection rootAddr)

casePair :: TIM ()
casePair =
  withDataArg $ \pairNode -> do
    case pairNode of
      Data 0 [arg1Addr, arg2Addr] -> do
        funAddr <- getArgAddr
        arg1AppAddr <- alloc (Application funAddr arg1Addr)
        updateRoot (Application arg1AppAddr arg2Addr)
      _ -> throwError "pattern match failed on case_pair"

caseList :: TIM ()
caseList =
  withDataArg $ \listNode -> do
    baseAddr <- getArgAddr
    stepAddr <- getArgAddr
    case listNode of
      Data 0 [] -> do
        updateRoot (Indirection baseAddr)
      Data 1 [arg1Addr, arg2Addr] -> do
        arg1AppAddr <- alloc (Application stepAddr arg1Addr)
        updateRoot (Application arg1AppAddr arg2Addr)
      _ -> throwError "pattern match failed in case_list"


printNum :: TIM ()
printNum = do
  no_dump <- dumpNull
  if no_dump then do
    withDataArg $ \outNode -> do
      num <- getNumber outNode
      output (show num)
      retAddr <- getArgAddr
      updateRoot (Indirection retAddr)
  else
    throwError "dump not empty"


abort :: TIM ()
abort = throwError "user aborted execution"
