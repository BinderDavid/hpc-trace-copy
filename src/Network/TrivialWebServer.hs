-- (c) Andy Gill

module Network.TrivialWebServer where

import System.Posix
import System.Posix.Signals
import Network
import IO 
import Monad 

import Control.Concurrent 
import Control.Exception as Exc
import Control.Concurrent.Chan
import qualified List
import qualified Char

-- Trivial web server, taking from the Cherry chess rendering software.
-- Takes callback for each request as an argument.

server :: Int -> Int -> Server -> IO ()
server threadCount portNo (Server { serve = serve }) = do
        installHandler sigPIPE Ignore Nothing
        chan <- newChan
        sequence [ forkIO (worker chan) | i <- take threadCount [0..]]
        sock  <- listenOn (PortNumber $ fromIntegral portNo)
        loopIO  
           (do (h,nm,port) <- accept sock
--             print (h,nm,port)
               writeChan chan h) `finally` sClose sock
        return ()
  where
      loopIO m          = do m
                             loopIO m

      worker chan = do
               tid <- myThreadId
               h <- readChan chan
               t <- hGetBuffering h            
--             print t
               ln <- IO.hGetLine h
--               print $ ">> " ++ show tid ++ ":" ++ ln
               case words ln of
                 ["GET",url,"HTTP/1.1"] 
                   -> do 
--                       print ("GET",url) 
                         serve file args $ Response 
				{ reply = sendMsg h "200 OK"
				, replyWithFailure = sendMsg h "400 Bad Request" False "text/html" 
				}
                      where (file,args) = splitup url
                 _ -> sendMsg h "400 Bad Request" False "text/html" $
                        "<html><body>Bad Request</body></html>\n"
               worker chan
                             
      sendMsg h code cache thing reply  
                       = (do -- print  $ "<< " ++ reply
                             hPutStr h $ "HTTP/1.1 " ++ code ++ "\r\n"
                             hPutStr h $ "Connection: close\r\n"
                             hPutStr h $ "Content-Type: " ++ thing ++ "\r\n"
                             hPutStr h $ "Content-Length: " ++ 
                                          show (length reply) ++ "\r\n"
                             hPutStr h $ "Cache-Control: " ++
                                            (if cache 
                                             then "max-age=3600"
                                             else "no-cache")
                                            ++ "\r\n"
                             hPutStr h $ "\r\n"
                             hPutStr h $ reply ++ "\r\n"
                             IO.hClose h
                                   -- we choose to ignore exceptions inside here
                             ) `Exc.catch` \ e -> do print "####################"
                                                     print e
                                                     return ()

splitup url = case span (/= '?') url of
                 (path,'?':args) -> (path,splitargs args)
                 (path,_) -> (path,[])
  where
    splitargs xs = case span (/= '=') xs of
                     (index,'=':rest) ->
                       case span (/= '&') rest of
                         (value,'&':rest') -> (index,clean value) : splitargs rest'
                         (value,_)         -> (index,clean value) : []
                     _ -> []  

    clean ('%':d1:d2:cs) 
                 = Char.chr (read $ "0x" ++ [d1,d2])  : clean cs
    clean (c:cs) = c : clean cs
    clean []     = []


-- These calls  may be asyncroynously(sp) done.
-- The contract is that you must return quickly (aka loading
-- a web page). If you have an expensive computation, you should
-- return, and call the reply continuation later when you are done.
-- This allows this thread to continue to service requests.

data Server = Server
    { serve :: String 			-- ^the URI
            -> [(String,String)] 	-- ^the arguments after questionmark
            -> Response 		-- ^the way to reply with a message
            -> IO ()
    }

data Response = Response
   { reply :: Bool  	-- ^ True => cache the result in the browser
           -> String 	-- ^ the content-type
           -> String    -- ^ the body
           -> IO ()
   , replyWithFailure 
           :: String 	-- ^ short html message
            -> IO ()
   }
