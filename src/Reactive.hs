module Reactive where

import Control.Concurrent
import Control.Exception as Exc
import Control.Concurrent.MVar

type Method s d = ((s -> IO s) -> IO ()) -> d
     
mkActor :: s -> Method s d -> IO d
mkActor initState body = do
   chan <- newChan

   let loop state = do body <- readChan chan
       	    	       state' <- body state
		       loop state'

   myForkIO $ loop initState

   return $ body (writeChan chan)

request :: (a -> s -> IO (r,s)) -> Method s (a -> IO r)
request fn send arg = do
   	     m  <- newEmptyMVar	
	     send (\ st -> do (rep,st') <- fn arg st
	     	  	      putMVar m rep
			      return st')
             takeMVar m 

mkActor3 :: s 
	 -> (     (forall r . (s -> IO (r,s)) -> IO r) 
	       -> (           (s -> IO s)     -> IO ()) 
	       -> actor)
	  -> IO actor
mkActor3 initState body = do
   state <- newMVar initState

   let reqs fn = do 
       s <- takeMVar state
       (r,s') <- fn s
       putMVar state s'
       return r

   let acts = undefined

   return $ body reqs acts

mkActor2 :: s 
	 -> (     (forall r . (s -> IO (r,s)) -> IO r) 
	       -> (           (s -> IO s)     -> IO ()) 
	       -> actor)
	  -> IO actor
mkActor2 initState body = do
   chan <- newChan

   let loop state = do body <- readChan chan
       	    	       state' <- body state
		       loop state'

   myForkIO $ loop initState

   let reqs fn = do
   	     m  <- newEmptyMVar	
	     writeChan chan $ \ st -> do (rep,st') <- fn st
	     	  	                 putMVar m rep
			                 return st'
             takeMVar m 

   let acts fn = do
       	    writeChan chan $ \ st -> fn st

   return $ body reqs acts

request2 :: ((s -> IO s) -> IO ()) -> (s -> IO (r,s)) -> IO r
request2 send fn = do
   	     m  <- newEmptyMVar	
	     send (\ st -> do (rep,st') <- fn st
	     	  	      putMVar m rep
			      return st')
             takeMVar m 

action :: (a -> s -> IO s) -> Method s (a -> IO ())
action fn send arg = send (fn arg)

test = mkActor ((99 :: Int))
           $ request (\ _a s -> return (show s,s + 1)) 

myForkIO m = do
	 forkIO (m `Exc.catch` \ e -> do
	 	   	       print "thread problem: "
	 	   	       print e
			       return ())

------------------------------------------------------------------------------

liftMethod :: ((a1 -> IO r1) -> (a2 -> IO r2) -> r)
	   -> Method s (a1 -> IO r1) 
	   -> Method s (a2 -> IO r2)
	   -> Method s r
liftMethod fn m1 m2 arg = fn (m1 arg) (m2 arg)

lift3Method :: ((a1 -> IO r1) -> (a2 -> IO r2) -> (a3 -> IO r3) -> r)
	   -> Method s (a1 -> IO r1) 
	   -> Method s (a2 -> IO r2)
	   -> Method s (a3 -> IO r3)
	   -> Method s r
lift3Method fn m1 m2 m3 arg = fn (m1 arg) (m2 arg) (m3 arg)
	   
