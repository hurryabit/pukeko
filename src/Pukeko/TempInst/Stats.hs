module Pukeko.TempInst.Stats
  ( Stats
  , tick
  , heapUsage
  , gcRun
  , gcVolume
  )
  where

import Text.Printf

data Stats = Stats
  { _ticks    :: !Int
  , _maxHeap  :: !Int
  , _gcRuns   :: !Int
  , _gcVolume :: !Int
  }

tick :: Stats
tick = mempty { _ticks = 1 }

heapUsage :: Int -> Stats
heapUsage usage = mempty { _maxHeap = usage }

gcRun :: Stats
gcRun = mempty { _gcRuns = 1 }

gcVolume :: Int -> Stats
gcVolume volume = mempty { _gcVolume = volume }


instance Show Stats where
  show (Stats { _ticks, _maxHeap, _gcRuns, _gcVolume }) =
    printf "Ticks = %d; Max Heap Usage = %d; GC Runs = %d; GC Volume = %d"
      _ticks _maxHeap _gcRuns _gcVolume

instance Monoid Stats where
  mempty = Stats { _ticks = 0, _maxHeap = 0, _gcRuns = 0, _gcVolume = 0 }
  s1 `mappend` s2 = 
    Stats
      { _ticks    = _ticks    s1 +     _ticks    s2
      , _maxHeap  = _maxHeap  s1 `max` _maxHeap  s2
      , _gcRuns   = _gcRuns   s1 +     _gcRuns   s2
      , _gcVolume = _gcVolume s1 +     _gcVolume s2
      }
