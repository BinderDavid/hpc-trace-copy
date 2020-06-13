module StreamHandle where

import System.Posix.Process
import System.Posix.IO
import System.Posix.Env
import System.IO
import System.Exit
import Control.Concurrent 

import System.Cmd

import Flags 

openStream :: Flags -> IO (Handle,Handle)
openStream flags =
  case mode flags of
    ExecMode exec args -> do

       (cmd_rd,cmd_wt) <- createPipe 
       (event_rd,event_wt) <- createPipe 

       pid <- getProcessID

       forkProcess $ do
         closeFd cmd_wt
         closeFd event_rd
	 t1 <- getEnv "HPCRIX"
	 unsetEnv "HPCRIX"
         setEnv "HPCRIX" (show event_wt ++ ":" ++ show cmd_rd) True
	 t2 <- getEnv "HPCRIX"
	 executeFile exec False args Nothing
	 -- should never return
	 return ()

       d_h <- fdToHandle event_rd
       c_h <- fdToHandle cmd_wt

       closeFd cmd_rd
       closeFd event_wt

       return (d_h,c_h)
    FileMode filename ->
        undefined

