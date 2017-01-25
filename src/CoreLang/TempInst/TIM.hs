module CoreLang.TempInst.TIM
  ( Node (..)
  , TIM
  , runTIM
  , resolve
  , isDataNode
  , mkNumber
  , getNumber
  , mkBool
  , getBool
  , module CoreLang.TempInst.Machine
  )
  where

import CoreLang.Language.Syntax (Identifier, Expr)
import CoreLang.TempInst.Machine
import CoreLang.TempInst.Stats (Stats)

type Environment = [(Identifier, Addr)]

type TIM = Machine Node Addr Environment Stats Environment

data Node
  = Marked      Node
  | Application Addr Addr
  | Function    Identifier [Identifier] Expr
  | Number      Integer
  | Indirection Addr
  | Primitive   Identifier (TIM ())
  | Constructor Int Int
  | Data        Int [Addr]


runTIM :: Int -> TIM a -> IO (Either String a, String, Stats)
runTIM heapSize tim = do
  (res, state, _, stats) <- runMachine tim heapSize [] []
  return (res, state, stats)


resolve :: Identifier -> TIM Addr
resolve ident = do
  global <- get
  local  <- ask
  case lookup ident (local ++ global) of
    Nothing   -> throwError ("unknown identifier " ++ ident)
    Just addr -> return addr


isDataNode :: Node -> Bool
isDataNode node =
  case node of
    Number _ -> True
    Data _ _ -> True
    _        -> False

mkNumber :: Integer -> Node
mkNumber = Number

getNumber :: Node -> TIM Integer
getNumber node =
  case node of
    Number num -> return num
    _          -> throwError ("no number: " ++ show node)

mkBool :: Bool -> Node
mkBool bool =
  case bool of
    False -> Data 0 []
    True  -> Data 1 []
  
getBool :: Node -> TIM Bool
getBool node =
  case node of
    Data 0 [] -> return False
    Data 1 [] -> return True
    _         -> throwError ("no boolean: " ++ show node)


instance Show Node where
  show node =
    case node of
      Application addr1 addr2 -> unwords [ "App", show addr1, show addr2 ]
      Function    fun _ _     -> unwords [ "Fun", fun ]
      Number      n           -> unwords [ "Num", show n ]
      Indirection addr        -> unwords [ "Ind", show addr ]
      Primitive   fun _       -> unwords [ "Pri", fun ]
      Constructor t n         -> unwords [ "Con", show t, show n ]
      Data        t addrs     -> unwords $ "Dat" : show t : map show addrs
      Marked      node        -> unwords [ "***",  show node, "***" ]
