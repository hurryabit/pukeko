module CoreLang.TempInst.Builtins
  ( constructors
  , primitives
  )
  where

import CoreLang.Language.Syntax (Identifier)
import CoreLang.TempInst.TIM

constructors :: [(Identifier, Int, Int)]
constructors =
  [ ("false"  , 0, 0)
  , ("true"   , 1, 0)
  , ("mk_pair", 0, 2)
  , ("nil"    , 0, 0)
  , ("cons"   , 1, 2)
  ]

primitives :: [(Identifier, TIM ())]
primitives =
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
  , ("not", not1)
  , ("&&" , and2)
  , ("||" , or2 )
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


not1 :: TIM ()
not1 = prim1 $ \node -> mkBool <$> not <$> getBool node

and2 :: TIM ()
and2 =
  withDataArg $ \arg1Node -> do
    arg1 <- getBool arg1Node
    arg2Addr <- getArgAddr
    let rsltNode = if arg1 then Indirection arg2Addr else mkBool False
    updateRoot rsltNode

or2 :: TIM ()
or2 =
  withDataArg $ \arg1Node -> do
    arg1 <- getBool arg1Node
    arg2Addr <- getArgAddr
    let rsltNode = if arg1 then mkBool True else Indirection arg2Addr
    updateRoot rsltNode


if_ :: TIM ()
if_ =
  withDataArg $ \condNode -> do
    cond <- getBool condNode
    thenAddr <- getArgAddr
    elseAddr <- getArgAddr
    let rsltAddr = if cond then thenAddr else elseAddr
    updateRoot (Indirection rsltAddr)

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
