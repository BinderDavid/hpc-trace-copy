-- The Hpc tracer viewer/debugger

module Main where

import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Util

import Control.Concurrent
import Control.Exception as Exc
import Control.Concurrent.MVar

import Network.TrivialWebServer
import Network.AjaxServer

import System.Environment (getArgs, withArgs)



import Reactive
import Common

import MixActor
import TixActor
import CodeRenderActor
import TixStreamActor
import TracerActor
import AjaxAPI

import CachedFiles

import Flags
import StreamHandle

------------------------------------------------------------------------------

main = do
       args <- getArgs     
       let flags = parseFlags args
       
       (d_h,c_h) <- openStream flags

       step <- tracerActor (hpcdir flags) (srcdir flags) d_h c_h

       let tracerActor :: TracerRequest -> IO [AjaxCallback]
           tracerActor req = do resp <- step req
       	   	       	        let respTxt = sendTracerResponse resp
--				print respTxt
       	   	       	        return $ respTxt

       toScreenRender <- codeRenderActor (hpcdir flags) (srcdir flags)

       let mkPage ty body = return $ PageResponse 200 False ty body

       let rpc = alts $
       	       [ call "boot" (tracerActor InitRequest)
{-
	       , call "step" (\ (n :: Int) -> tracerActor (StepRequest Forward))
	       	      	     <*> jsonArg "count"
-}
	       , call "step/forward"    (tracerActor (StepRequest Forward))
	       , call "step/backward"   (tracerActor (StepRequest Backward))
	       , call "run/forward"     (tracerActor (RunRequest Forward))
	       , call "run/backward"    (tracerActor (RunRequest Backward))
	       , call "run/backward"    (tracerActor (RunRequest Backward))
	       , call "please/continue" (tracerActor ContinueRequest)
	       , call "please/stop"     (tracerActor StopRequest)
	       ] ++
	       [ jsonArg "status" <*>
	       	 jsonArg "breakpoint" <*>
		      call ("breakpoint")
		      ( \ bp onOff -> tracerActor 
		      	  	     $ (if onOff then SetBreakPoint 
				       	   	 else ClearBreakPoint)
		      	  	     $ bp
		      )
               ]  {-  ++

	       [ jsonArg "name" <*>
	       	 call "usertext" (\ name value -> do print (name::String,value::String)
	       	      		    	       	     return [])
       	       ]  -}
	       -- later, implement http://..:8091/modules/Foo as the render engine
       let pageRpc = alts
               [ jsonArg "module" <*>
	         call "modulecode" (\ mod -> do
	       	      	     	txt <- toScreenRender mod
				mkPage "text/html" txt)
	       ]


       mVar <- newEmptyMVar 

       let pages :: String -> IO PageResponse
           pages "/favicon.ico" = mkPage "text/ico"  favicon
           pages "/progress.gif" = mkPage "image/gif"  progress
           pages "/"            = mkPage "text/html" root_html
	   pages "/code.html"   = mkPage "text/html" code_html
           pages "/header.html" = mkPage "text/html" header_html
           pages "/status.html" = mkPage "text/html" status_html
           pages "/footer.html" = mkPage "text/html" footer_html
           pages "/default.css" = do
	   	 		  -- hack to improve compile cycle.
--	   	 		  default_css <- readFile "/Users/andy/darcs/hpc/tools/tracer/fs/default.css"
				  mkPage "text/css"  default_css
           pages "/default.js"  = do
	   	 		  -- hack to improve compile cycle.
--	   	 		  default_js <- readFile "/Users/andy/darcs/hpc/tools/tracer/fs/default.js"
	   	 		  mkPage "text/js"   default_js
           pages "/quit"	= do putMVar mVar ()
	   	 		     mkPage "text/html" "aborting"
	   pages other          = do print other
	   	 		     mkPage "text/html" other

       forkIO $ ajaxServer (port flags) rpc pageRpc pages

       takeMVar mVar
       return ()
