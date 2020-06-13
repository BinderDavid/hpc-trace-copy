module BreakPoint where

import Common
import TixActor 
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)

setBreakPoint   :: BreakPoint -> BreakPoints -> BreakPoints
setBreakPoint bp bps 
  | bp `elem` bps = bps
  | otherwise     = bps ++ [bp]
clearBreakPoint :: BreakPoint -> BreakPoints -> BreakPoints
clearBreakPoint bp bps = filter (/= bp) bps

-- This is where we lookup the module, local tick number, 
-- at turn it into a complete tick info structure.

normalizeBreakPoint :: TixActor -> BreakPoint -> IO BreakPoint
normalizeBreakPoint tixActor (ReqTickBox modName localTickId) = do
     tickInfo <- lookupLocalTickInfo tixActor modName localTickId
     return $ TickBox tickInfo
normalizeBreakPoint tixActor other = return $ other

isBreakPoint :: TracerState -> Bool
isBreakPoint = or . checkBreakPoints

checkBreakPoints :: TracerState -> [Bool]
checkBreakPoints state = map isBP (tracerBreakPoints state)
  where
     counter = tracerCounter state
     isBP (CounterAt count) = counter == count
     isBP (AllExceptions)   = case tracerEvent state of
     	  		        Raise -> True
				_ -> False
{-
     isBP (ThreadChange)    = case tracerEvent state of
     	  		        Thread {} -> True
				_ -> False
     isBP (ThreadChangeTo id1) 
     	  		     = case tracerEvent state of
     	  		        Thread id2 -> id1 == id2
				_ -> False
-}
     isBP (ThreadTermination)
			    = case tracerEvent state of
     	  		        ThreadFinished -> True
				_ -> False
     isBP (TickBox info)    = case tracerEvent state of
     	  		        Tick i -> i == tbiGlobalTickNo info
				_ -> False

     isBP _                 = False



nextCounterBreakPoint :: TracerState -> Maybe Integer
nextCounterBreakPoint state =
    case counter_bps of
      [] -> Nothing
      (n:_) -> Just n
  where
    counter_bps = sort 
                    [ n | CounterAt n <- tracerBreakPoints state
		          -- must be in the future
    	      	        , n > tracerCounter state
		    ]


tickIdBreakPointSet :: TracerState -> Set GlobalTickId
tickIdBreakPointSet state = Set.unions $
   [ Set.singleton (tbiGlobalTickNo info) | TickBox info <- tracerBreakPoints state ]
		    		    