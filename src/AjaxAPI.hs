module AjaxAPI 
       ( module AjaxAPI
       , AjaxCallback
       ) where

import Network.AjaxServer
import Common
import Trace.Hpc.Util (HpcPos, toHpcPos, fromHpcPos)
import Trace.Hpc.Mix  
import qualified Data.JSON as JSON
import Debug.Trace
import Data.Maybe
import Utils

-- The new API

setCounter :: Integer -> AjaxCallback
setCounter counter = 
	  ajaxCallback "setCounter"
	    <@> showWithCommas counter

	    -- Start using Strings, for when we can name threads
setThreadID :: Int -> AjaxCallback
setThreadID tid = 
	  ajaxCallback "setThreadID"
 	    <@> show tid
	    
setEvent :: Event -> AjaxCallback
setEvent event      = ajaxCallback "setEvent" <@> event

------------------------------------------------------------------------------

setTracerState :: TracerState -> [AjaxCallback]
setTracerState state =
  [ setCounter 		$ tracerCounter state
  , setThreadID		$ tracerThreadID state
  , setEvent		$ tracerEvent state
  , setRunning		$ tracerRunning state
  ] ++
    (setBreakPoints	$ tracerBreakPoints state)

setTicked :: Maybe TickBoxInfo -> AjaxCallback
setTicked Nothing       = ajaxCallback "setTicked" <@> JSON.Null
setTicked (Just ticked) = ajaxCallback "setTicked" <@> ticked

setBreakPoints :: BreakPoints -> [AjaxCallback]
setBreakPoints bps = 
  [ ajaxCallback "clearBreakPoints" 
  ] ++ 
  [ setBreakPoint bp 
  | bp <- bps 
  ]

setBreakPoint :: BreakPoint -> AjaxCallback
setBreakPoint bp = ajaxCallback "setBreakPoint2" <@> bp

setBreakPointLights :: [Bool] -> AjaxCallback
setBreakPointLights bps = ajaxCallback "setBreakPointLights" <@> bps

setRunning :: Maybe Direction ->  AjaxCallback
setRunning Nothing         = ajaxCallback "setRunning" <@> "Stopped"
setRunning (Just Forward)  = ajaxCallback "setRunning" <@> "Forward"
setRunning (Just Backward) = ajaxCallback "setRunning" <@> "Backward"

------------------------------------------------------------------------------
--

sendTracerResponse :: TracerResponse -> [AjaxCallback]
sendTracerResponse (TracerStateResponse state) = setGUIState state
sendTracerResponse (MultiResponse responses)   = concatMap sendTracerResponse responses
sendTracerResponse (ModuleListResponse mods)   = [ ajaxCallback "setModules" <@> mods ]
sendTracerResponse (RunResponse count)         = [ ajaxCallback "running" <@> showWithCommas count ]

------------------------------------------------------------------------------

setGUIState :: GUIState -> [AjaxCallback]
setGUIState (GUIState state ticked bps) =
  setTracerState state ++ 
  [setTicked ticked,
  setBreakPointLights bps]

------------------------------------------------------------------------------
-- These encodings have to be respected by the JavaScript.
-- This code is the cannonical specification of encodings.

instance JSON BreakPoint where
  toJSON (AllExceptions)      = JSON.String "AllExceptions"
  toJSON (ThreadChange)       = JSON.String "ThreadChange"
  toJSON (ThreadTermination)  = JSON.String "ThreadTermination"
  toJSON (ThreadChangeTo n)        = JSON.Object $ JSON.fromList 
  	 	    	      	   [ ("tag",JSON.String "ThreadChangeTo")
				   , ("tid", JSON.String $ show n) -- yes, a string
				   ]
  toJSON (CounterAt n)        = JSON.Object $ JSON.fromList 
  	 	    	      	   [ ("tag",JSON.String "CounterAt")
				   , ("count", JSON.String $ showWithCommas n) -- yes, a string
				   ]
  toJSON (TickBox tInfo)  = JSON.Object $ JSON.fromList 
  	 	    	      	   [ ("tag",JSON.String "TickBox")
  	 	    	      	   , ("tickInfo",toJSON tInfo)
				   ]
  toJSON (ReqTickBox mod' id')  = JSON.Object $ JSON.fromList 
  	 	    	      	   [ ("tag",JSON.String "ReqTickBox")
  	 	    	      	   , ("module",JSON.String $ mod')
				   , ("id", JSON.Int $ id')
				   ]
  fromJSON (JSON.String "AllExceptions") = return AllExceptions
  fromJSON (JSON.String "ThreadChange") = return ThreadChange
  fromJSON (JSON.String "ThreadTermination") = return ThreadTermination
  fromJSON (JSON.Object o) = listToMaybe $ catMaybes 
      [ do JSON.String "CounterAt" <- JSON.lookup "tag" o
	   JSON.String n           <- JSON.lookup "count" o
	   () <- trace (show n) $ return ()
	   case reads (filter (/= ',') n)  of
	     [(v,"")] -> return (CounterAt v)
	     _ -> Nothing
      , do JSON.String "ThreadChangeTo" <- JSON.lookup "tag" o
	   JSON.String n                <- JSON.lookup "tid" o
	   () <- trace (show n) $ return ()
	   case reads n of
	     [(v,"")] -> return (ThreadChangeTo v)
	     _ -> Nothing
      , do JSON.String "TickBox"    <- JSON.lookup "tag" o
	   tickInfoJSON             <- JSON.lookup "tickInfo" o

	   tickInfo 	            <- fromJSON tickInfoJSON
	   return $ TickBox tickInfo
      , do JSON.String "ReqTickBox" <- JSON.lookup "tag" o
	   JSON.String mod'          <- JSON.lookup "module" o
	   JSON.Int id'              <- JSON.lookup "id" o
	   return $ ReqTickBox mod' id'
      ]
  fromJSON _ = Nothing


instance JSON TickBoxInfo where
  toJSON (TickBoxInfo mod' loc local_no global_no ty) = JSON.Object $ JSON.fromList 
  		    	      	   [ ("module",   JSON.String mod')
  	 	    	      	   , ("location", toJSON loc)
  	 	    	      	   , ("local",     JSON.Int local_no)
  	 	    	      	   , ("global",   JSON.Int global_no)
				   , ("tickType", toJSON ty)
				   ]

  fromJSON (JSON.Object o) = do
  	   mod'      <- findJSON "module" o
	   loc       <- findJSON "location" o
	   local_no  <- findJSON "local" o
	   global_no <- findJSON "global" o
	   ty        <- findJSON "tickType" o
	   return $ TickBoxInfo mod' loc local_no global_no ty
  fromJSON _ = Nothing

instance JSON HpcPos where 
  toJSON pos = JSON.Object $ JSON.fromList 
  		    	      	   [ ("startLine",JSON.Int startLine)
  	 	    	      	   , ("startCol", JSON.Int startCol)
  	 	    	      	   , ("endLine",  JSON.Int endLine)
				   , ("endCol",   JSON.Int endCol)
				   ]
     where (startLine,startCol,endLine,endCol) = fromHpcPos pos
  fromJSON (JSON.Object o) = do
           startLine <- findJSON "startLine" o
           startCol <- findJSON "startCol" o
           endLine <- findJSON "endLine" o
           endCol <- findJSON "endCol" o
	   return $ toHpcPos (startLine,startCol,endLine,endCol)
  fromJSON _ = Nothing

instance JSON BoxLabel where 
  toJSON (ExpBox False) = JSON.String "ExpBox"
  toJSON (ExpBox True)  = JSON.String "AltBox"
  toJSON (TopLevelBox path) = JSON.Object $ JSON.fromList 
  	 	      	           [ ("tag",  JSON.String "TopLevelBox")
				   , ("path", toJSON $ path)
				   ]
  toJSON (LocalBox path) = JSON.Object $ JSON.fromList 
  	 	      	           [ ("tag",  JSON.String "LocalBox")
				   , ("path", toJSON $ path)
				   ]
  toJSON (BinBox GuardBinBox bool) = JSON.Object $ JSON.fromList 
  	 	      	           [ ("tag",  JSON.String "GuardBinBox")
				   , ("value", toJSON $ bool)
				   ]
  toJSON (BinBox CondBinBox bool) = JSON.Object $ JSON.fromList 
  	 	      	           [ ("tag",  JSON.String "CondBinBox")
				   , ("value", toJSON $ bool)
				   ]
  toJSON (BinBox QualBinBox bool) = JSON.Object $ JSON.fromList 
  	 	      	           [ ("tag",  JSON.String "QualBinBox")
				   , ("value", toJSON $ bool)
				   ]
  toJSON _ = error "toJSON"  	 	             

  fromJSON (JSON.String "ExpBox") = return $ ExpBox False
  fromJSON (JSON.String "AltBox") = return $ ExpBox True
  fromJSON (JSON.Object o) = altsJSON 
    [ do "TopLevelBox" <- findJSON "tag" o
         path <- findJSON "path" o
         return $ TopLevelBox path
    , do "LocalBox" <- findJSON "tag" o
         path <- findJSON "path" o
         return $ LocalBox path
    , do "GuardBinBox" <- findJSON "tag" o
         value <- findJSON "value" o
         return $ BinBox GuardBinBox value
    , do "CondBinBox" <- findJSON "tag" o
         value <- findJSON "value" o
         return $ BinBox CondBinBox value
    , do "QualBinBox" <- findJSON "tag" o
         value <- findJSON "value" o
         return $ BinBox QualBinBox value
   ]
  fromJSON _ = error "fromJSON"

instance JSON Event where 
  toJSON (Tick tick)       = JSON.Object $ JSON.fromList 
  	 	      	           [ ("tag",   JSON.String "Tick")
				   , ("tick",  JSON.Int tick)
				   ]
  toJSON (Raise)           = JSON.String "Raise"
  toJSON (ThreadFinished)  = JSON.String "ThreadFinished"
  toJSON (Finished)        = JSON.String "Finished"
  fromJSON _ = error "fromJSON"