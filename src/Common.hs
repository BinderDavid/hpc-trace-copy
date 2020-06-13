module Common where

import Trace.Hpc.Util (HpcPos)
import Trace.Hpc.Mix (BoxLabel)

------------------------------------------------------------------------------
-- |An @Event@ is an item in the stream from the debugged process.

-- our debugging trail is a list of events.
data Event = Tick !Int		-- The global tick number
     	   | Raise		-- Any Raised exception
	   | ThreadFinished	-- last event in a Thread
	   | Finished		-- final event

------------------------------------------------------------------------------
-- |@TracerRequest@ are requests made by the Ajax interface of the debuggger.

data TracerRequest 
           = StepRequest Direction
	   | InitRequest	-- ^called at initialization time, to set things up
     	   | RunRequest Direction  -- until a breakpointable event
	   | SetBreakPoint BreakPoint
	   | ClearBreakPoint BreakPoint
	   | ContinueRequest		-- ^keep running (looking for a breakpoint)
	   | StopRequest		-- ^stop please (looking for a breakpoint)

data Direction = Forward | Backward
     deriving Show

------------------------------------------------------------------------------
-- |The @TracerState@ is the micro-architecture of the debugger; what information
--  we hold onto from step to step.

data TracerState = TracerState
           { tracerCounter	:: !Integer	-- ^the cursor
	   , tracerThreadID	:: !ThreadID	-- ^what thread is currently active
	   , tracerEvent	:: !Event	-- ^The current event
           , tracerBreakPoints  :: !BreakPoints
	   , tracerRunning      :: Maybe Direction -- ^looking for a breakpoint
	   }
      deriving Show

-- Consider include the global id as well as the local id here.
data TickBoxInfo = TickBoxInfo
          { tbiModule 	     :: !String		-- ^what module is at this cursor
	  , tbiLocation	     :: !HpcPos		-- ^where is the cursor in the source?
	  , tbiLocalTickNo   :: !LocalTickId 	-- ^local tick number, for displaying
	  , tbiGlobalTickNo  :: !GlobalTickId 	-- ^global tick number
	  , tbiTickType	     :: !BoxLabel	-- ^possible style of highlight
	  }
      deriving (Show, Eq, Ord)

type BreakPoints = [BreakPoint]	-- does not include any abrevited breakpoints.

data BreakPoint  
  = AllExceptions
  | ThreadChange
  | ThreadTermination
  | ThreadChangeTo ThreadID
  | CounterAt Integer
  | TickBox TickBoxInfo

-- These are abreviated versions sent from the GUI, 
--  which does not have access to things like tick box location information.
  | ReqTickBox String LocalTickId

    deriving (Show, Eq, Ord)

------------------------------------------------------------------------------
-- This is basically the TracerState, with some derived infomation.

data GUIState = GUIState 
     { guiTracerState	 :: TracerState
     , guiTracerTicked   :: Maybe TickBoxInfo	-- which tick box to light up
     , guiTracerBreakers :: [Bool]		-- which breakpoints to light up
     }

------------------------------------------------------------------------------
-- The @TracerResponse@ is what messages get sent back to the GUI about
-- changes in the TracerState.

data TracerResponse
     = TracerStateResponse !GUIState	-- ^The brute force update; just update everything!
     | MultiResponse [TracerResponse]	-- ^For when you want to send several responses
     | ModuleListResponse [String]	-- ^A list of all possible modules in this program
     | RunResponse !Integer		-- ^The curent cursor when looking for a breakpoint

type ThreadID     = Int		-- Threads later will be strings
type GlobalTickId = Int		-- tick number inside a specific executable
type LocalTickId  = Int		-- tick number inside a specific module

------------------------------------------------------------------------------

instance Show Event where
   show (Tick n) = show n
   show (Raise)  = "Raise"
   show (ThreadFinished) = "ThreadFinished"
   show (Finished) = "Finished"

instance Read Event where
   readsPrec _p ('R':'a':'i':'s':'e':xs)
				= [(Raise,xs)]
   readsPrec _p ('T':'h':'r':'e':'a':'d':'F':'i':'n':'i':'s':'h':'e':'d':xs) 
				= [(ThreadFinished,xs)]
   readsPrec _p ('F':'i':'n':'i':'s':'h':'e':'d':xs)
			        = [(Finished,xs)]
   readsPrec p xs 		= [ (Tick n,ys) | (n,ys) <- readsPrec p xs ]
   			      	 

