-- TODO: renamed as RixStreamActor

module TixStreamActor (readTickActor, StreamActorResult(..), finished) where

import Data.List as List
import System.IO
import Reactive

import Common

import Debug.Trace
import Data.Set(Set)
import qualified Data.Set as Set

data State = State 
     { cursor 	  :: ThreadedEvent
     , count      :: !Integer			-- where we are	
     , target     :: !Integer			-- where we are going to
     , before     :: ![ThreadedEvent]
     , after      :: ![ThreadedEvent]
     , stopAtTicks :: Set GlobalTickId
     , remoteStopAtTicks 
	          :: Set GlobalTickId
     } deriving (Show)


finished :: State -> Bool
finished state = case cursor state of
	           ThreadedEvent Finished _ -> True
    	           _ -> False 

-- Move this into TixStream.
data ThreadedEvent = ThreadedEvent !Event !ThreadID
      deriving (Show)

addEvent :: Event -> State -> ThreadedEvent
addEvent ev (State { cursor = ThreadedEvent _ tid })  = ThreadedEvent ev tid

data StreamActorResult 
   = StreamActorResult 
     { counter  :: !Integer		-- ^global tick counter
     , event    :: !Event 		-- ^event at the cursor
     , threadId :: !ThreadID		-- ^the current thread (before/at the event)
     , live     :: !Bool		-- ^is this the live event (False == historical result)
     }

-- move forward or back, finding the tick number.
readTickActor :: Handle -> Handle -> IO (Integer -> Set GlobalTickId -> IO StreamActorResult)
readTickActor d_handle cmd_handle = do
  -- Will this help
--  t <- hGetBuffering d_handle
--  print t

--  hSetBuffering d_handle (LineBuffering)
--  hSetBuffering cmd_handle (LineBuffering)

  tixTxt <- hGetLine d_handle
  let (0,threadId',event') = parseEvent tixTxt

  let initState = 
        State { cursor = ThreadedEvent event' threadId'
      	      , count = 0
	      , target = 0
  	      , before = []
	      , after = []
	      , stopAtTicks = Set.empty
	      , remoteStopAtTicks = Set.empty
	      }

  let pull' :: Integer -> Set GlobalTickId -> State -> IO State
      pull' n ticks state = pull $ state { target = n
      	      	  	       	         , stopAtTicks = ticks
					 }

      -- check for breakpoint
      check :: State -> IO State
      check state@State { stopAtTicks = stopTicks, cursor = ThreadedEvent (Tick tickid) _ }
         | tickid `Set.member` stopTicks = return state
      check state = pull state

      pull :: State -> IO State
      pull state@State { target = n , count = c } 
--        | trace (show state) False = undefined

	  -- reached target
      	| n == c = return state

	| n > c && null (after state) && not (finished state) = do
	         -- We need to read a new entry from the remote process
		 -- set break at the target
		 hPutStrLn cmd_handle ("c" ++ show n) -- set counter bp
		 sequence [ do hPutStrLn cmd_handle ("u" ++ show x) 
--		 	       print ("unseting bp at ",x)
		 	  | x <- Set.elems $ remoteStopAtTicks state
			  ]
		 sequence [ do hPutStrLn cmd_handle ("s" ++ show x) 
--		 	       print ("seting bp at ",x)
		 	  | x <- Set.elems $ stopAtTicks state
			  ]
		 hPutStrLn cmd_handle ("")            -- run
		 hFlush cmd_handle
		 	   
--                 print ("waiting for message")
		 -- and wait for the breakpoint to activate
		 tixTxt <- hGetLine d_handle
--                 print ("got : ",tixTxt)
--		 print tixTxt
		 let (count',threadId',event') = parseEvent tixTxt

		 let before' = if c == count' - 1
		     	       then cursor state : before state
  			       else []

                 -- something interesting happened so we return anyway.

		 return $ state { cursor = ThreadedEvent event' threadId' 
		     	       , count = count'
			       , before = before'
			       , remoteStopAtTicks = stopAtTicks state
			       }

	| n > c && not (null $ after state) = 
                 check (state { cursor = head $! after state 
		      	      , count  = count state + 1
			      , before = cursor state : before state
			      , after  = tail $! after state
			      })

	| n < c && (null $ before state) && (current_loc /= 0) = do
	         -- we want to rewind here, so we pull some history
		 hPutStrLn cmd_handle ("h")           -- history
		 hFlush cmd_handle

		 befores <-
		   sequence [ do tixTxt <- hGetLine d_handle
--		                 print ("got(h): ",tixTxt)
			         let (count',threadId',event') = parseEvent tixTxt
			         if count' /= ix 
			           then error $ "strangeness" ++ show (ix,count')
			           else return $ ThreadedEvent event' threadId'
		 	    | ix <- [(max 0 (current_loc - 1024)) .. (current_loc - 1) ]
			    ]

                 pull $ state { before = drop (length $ after state) $ reverse befores }

	| n < c && not (null $ before state) = 
	         check (state { cursor = head $! before state 
		      	      , count  = count state - 1
			      , before = tail $! before state
			      , after  = cursor state : after state
			      })

	| otherwise = return state

       where 
    	  current_loc = c + fromIntegral (length (after state))


  mkActor2 initState $ \ request action targetCount tickBreakPoints ->
       request $ \ state -> do
	  state' <- pull' targetCount tickBreakPoints state
	  let (ThreadedEvent event tid) = cursor state'
	  let isLive = not (List.null $ after state')
	  return (StreamActorResult (count state') event tid isLive,state')


parseEvent :: String -> (Integer,ThreadID,Event)
parseEvent xs = case words xs of
	         ["Event",counter,tid,op] 
		   -> (read counter, read tid, read op)
		 _ -> error $ "parseEvent failure: " ++ show xs



