module CoreLang.TempInst.GC
  ( run
  )
  where

import CoreLang.TempInst.TIM

run :: TIM ()
run = do
  -- debug "before gc"
  -- debugState
  mapStack markFrom
  mapDump markFrom
  global <- get
  global <- mapM (\(x, addr) -> (,) x <$> markFrom addr) global
  put global
  -- debug "between gc"
  -- debugState
  scan
  -- debug "after gc"
  -- debugState

markFrom :: Addr -> TIM Addr
markFrom addr = do
  node <- deref addr
  case node of
    Marked      _ -> return ()
    Indirection _ -> return ()
    _             -> update addr (Marked node)
  case node of
    Marked _ -> return addr
    Application addr1 addr2 -> do
      addr1 <- markFrom addr1
      addr2 <- markFrom addr2
      update addr (Marked (Application addr1 addr2))
      return addr
    Indirection addr1 -> do
      addr1 <- markFrom addr1
      update addr (Indirection addr1)
      return addr1
    Data t addrs -> do
      addrs <- mapM markFrom addrs
      update addr (Marked (Data t addrs))
      return addr
    Number      _     -> return addr
    Function    _ _ _ -> return addr
    Primitive   _ _   -> return addr
    Constructor _ _   -> return addr

scan :: TIM ()
scan = do
  addrs <- usedAddrs
  forM_ addrs $ \addr -> do
    node <- deref addr
    case node of
      Marked node -> update addr node
      _           -> free addr
