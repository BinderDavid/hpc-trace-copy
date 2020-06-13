module TixActor where

import Trace.Hpc.Mix

import Data.Array.MArray
import Data.Array.IO

import Reactive
import MixActor

import Common

data TixActor = TixActor 
     { lookupGlobalTickInfo :: GlobalTickId -> IO TickBoxInfo
     , lookupLocalTickInfo  :: String -> LocalTickId -> IO TickBoxInfo
     }

globalTixActor :: String 
	       -> String 
	       -> [(String,Int)] 
	       -> IO TixActor
globalTixActor hpcDir srcDir modSizes = do
  getMix <- mixActor hpcDir srcDir

  let modOffsets = 0 : zipWith (+) (map snd modSizes) modOffsets 

  let modInfo    = [ (mod,(sz,offset))
  	           | ((mod,sz),offset) <- zip modSizes modOffsets
	           ]
  	           
--  print modInfo

  let maxTixId = sum (map snd modSizes) - 1

  -- fill the array with module name initially

  arr <- newListArray (0::Int,maxTixId) $
      	    	  concat [ [ Left mod | _ <- take sz [0..] ]
		  	 |  (mod,sz) <- modSizes
			 ]

  let _types = (arr :: IOArray Int (Either String TickBoxInfo))

  let fillIn modName = do
  	 case lookup modName modInfo of
	   Nothing -> print $ "can not find : " ++ show modName
	   	      
	   Just (sz,offset) -> do
	   	mix@(Mix _ _ _ _ mix') <- getMix modName
		-- assert length mix' == sz
--	        print ("writing",offset,length mix' + offset - 1,sz,length mix',modName)
	   	sequence_ [ do writeArray arr ix $ Right $ TickBoxInfo 
			       		      	 	     modName
							     loc
							     lix
						             ix
							     ty
			  | (lix,ix,(loc,ty)) <- zip3 [0..] [offset..] mix'
			  ]

  let findVal :: GlobalTickId -> IO TickBoxInfo
      findVal n = do
       val <- readArray arr n
       case val of
         Left modName -> do 
	      fillIn modName 
	      val' <- readArray arr n
	      case val' of
	        Left modName' -> do
		     print ("problem with loading array" ++ show (val,val',n))
		     error ""
		Right val' -> return $ val'
	 Right val -> return $ val
                
  let initState = ()

  mkActor2 initState (\ request action -> TixActor
     { lookupGlobalTickInfo = \ globId -> request $ \ () -> do
	      	       	  --	print ("glob=",globId)
			  	val <- findVal globId
			  --	print ("find:",val)
			  	return (val,())
     , lookupLocalTickInfo = \ modName localId -> request $ \ () -> do
       			        case lookup modName modInfo of
				  Just (_,offset) -> do val <- findVal (localId + offset)
				       		     	return (val,())
				  Nothing -> error $ "bad module" ++ modName
     })
