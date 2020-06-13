module TracerActor where

import Trace.Hpc.Mix
import Trace.Hpc.Util

import System.IO
import qualified Data.Set as Set

import TixActor
import TixStreamActor

import Reactive
import Common
import BreakPoint

import Control.Concurrent

-- The main actor

data State = State
  { count :: Integer
  }

type TracerActorState = (Maybe TracerState,TracerState)


tracerActor :: String 
	    -> String 
	    -> Handle -> Handle -> IO (TracerRequest -> IO TracerResponse)
tracerActor hpcDir srcDir d_handle cmd_handle = do 
  starts <- hGetLine d_handle
  mods <- hGetLine d_handle

  getTix <- readTickActor d_handle cmd_handle

  StreamActorResult { counter = count
	   	    , event = tixInfo
		    , threadId = tid
		    , live = isLive
		    } <- getTix 0 (Set.empty)

   -- We actually store a pair of states, 
   --   - the current state according to the GUI,
   --   - and the current state.

  let initState = TracerState 
        { tracerCounter = count
	, tracerThreadID = tid
	, tracerEvent = tixInfo
	, tracerBreakPoints = []
	, tracerRunning = Nothing
	}

--  print mods
  
  let send v = return (v,0)

  let modSizes :: [(String,Int)]
      modSizes = read mods
 
     -- we will need to improve this in the future
  tixActor <- globalTixActor hpcDir srcDir modSizes


  let record Nothing newState = do
      	   	   return ( MultiResponse
			     [ ModuleListResponse (map fst modSizes)
			     , TracerStateResponse (GUIState newState Nothing [])
			     ]
			   , (Just newState,newState)
			   )

      record (Just oldState) newState = do
	   opt_ticked <- 
	       case tracerRunning newState of
	          Nothing -> 
		     case tracerEvent newState of
	               Tick tixNo -> do
		      	  tixInfo <- lookupGlobalTickInfo tixActor tixNo
			  return $ Just tixInfo
                       _ -> return $ Nothing
                  _ -> return $ Nothing
--           print (oldState,newState)
           return ( TracerStateResponse $
	   	       GUIState 
		         { guiTracerState    = newState
			 , guiTracerTicked   = opt_ticked
			 , guiTracerBreakers = checkBreakPoints newState
			 }
	   	  , (Just newState,newState)
		  )


  let setBP bp state0 = do
	   bp' <- normalizeBreakPoint tixActor bp
           let state1 = state0 { tracerBreakPoints = setBreakPoint bp' $ tracerBreakPoints state0 }
--	   print $ tracerBreakPoints state1
	   return state1

  let clearBP bp state0 = do
	   bp' <- normalizeBreakPoint tixActor bp
           let state1 = state0 { tracerBreakPoints = clearBreakPoint bp' $ tracerBreakPoints state0 }
--	   print $ tracerBreakPoints state1
	   return state1

  let dir Forward = 1
      dir Backward = -1

  let step d state0 = do
--      	   print "step(1)"
	   StreamActorResult { counter = count
	   		     , event = tixInfo
			     , threadId = tid
			     } <- getTix (fromIntegral d + tracerCounter state0) 
			       	  	 (tickIdBreakPointSet state0)

--      	   print "step(2)"
--	   print ("284",count,tixInfo)

	   let state1 = state0 { tracerCounter = count
	       	      	       , tracerThreadID = tid
			       , tracerEvent = tixInfo
			       }
           return state1

      searchForBP :: Direction
      		  -> ((TracerActorState -> IO TracerActorState) -> IO ()) 
		  -> TracerActorState
		  -> IO TracerActorState
      searchForBP d action (oldState,state0 @ TracerState { tracerRunning = Nothing }) 
      		= return $ (oldState,state0)

      searchForBP d action (oldState,state0) = do
      		  let steps = 314159 -- large(ish) prime, also pi * 100,000
      		  let target = ((dir d * steps + tracerCounter state0) `div` steps) * steps
		  let next_bp = case nextCounterBreakPoint state0 of
		      	          Nothing -> target
				  Just target' -> min target' target
                  res <- getTix next_bp (tickIdBreakPointSet state0)
      		  case res of
		    StreamActorResult 
		             { counter = count
	   		     , event = tixInfo
			     , threadId = tid
			     } -> do
			 let state1 = state0 { tracerCounter = count
	       	      	     	             , tracerThreadID = tid
					     , tracerEvent = tixInfo
			                     }

                         case tixInfo of
			    _ | isBreakPoint state1   -> return $ (oldState, state1 { tracerRunning = Nothing })
			    Finished                  -> return $ (oldState, state1 { tracerRunning = Nothing })
			    _ | otherwise -> do
		     	           action (searchForBP d action)
		                   return $ (oldState,state1)
	            _ -> error "bad actor"

      initRunToBP d action state0 = do
      		   action (searchForBP d action)
		   let state1 = state0 { tracerRunning = Just $ d }
		   return state1

  mkActor2 (Nothing,initState) $ \ request action req ->  
  	   request $ \ (state0,state1) -> do
	   state2 <- case req of
	               InitRequest          -> return $ state1
	               StepRequest Forward  -> step 1      state1
	               StepRequest Backward -> step (-1)   state1
	               RunRequest Forward   -> initRunToBP Forward action state1
	               RunRequest Backward  -> initRunToBP Backward action state1
	               (SetBreakPoint bp)   -> setBP   bp  state1
	               (ClearBreakPoint bp) -> clearBP bp  state1
	               ContinueRequest      -> return $ state1
	               StopRequest          -> return $ state1 { tracerRunning = Nothing }

           record state0 state2

------------------------------------------------------------------------------

showUsingXml :: [(String,[(String,String)])] -> String
showUsingXml cmds = 
 "<?xml version=\"1.0\"?>" ++
    "<cmds>" ++ 
    unlines [ "<" ++ cmd ++ concatMap showArgs args ++ "/>" | (cmd,args) <- cmds ] ++
    "</cmds>\n"
  where showArgs (nm,val) = " " ++ nm ++ "=\"" ++ val ++ "\""

