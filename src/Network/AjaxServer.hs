module Network.AjaxServer 
       ( (<*>)
       , call
       , intArg
       , boolArg
       , stringArg
       , readArg
       , jsonArg
       , AjaxParser
       , ajaxServer
       , withMethod
       , alts
       , PageResponse(..)
       , AjaxCallback		-- abstract
       , (<@>)
       , ajaxCallback
       , JSON(..)		-- a class
       , altsJSON
       , findJSON
       ) where 



import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.JSON as JSON
import Control.Monad
import Network.TrivialWebServer

------------------------------------------------------------------------------

-- This is our dictionary like thing.
data AjaxTypeDict a = AjaxTypeDict
     { decodeATD :: String -> Either String a
     , encodeATD :: String -> String
     , nameATD   :: String
     }

data AjaxAPI = API_Name String
     	     | forall a . API_Arg  String (AjaxTypeDict a) -- argument name and type
	     | API_Join AjaxAPI AjaxAPI
	     | API_Alt [AjaxAPI]


data Request = Request 
     { reqMethod :: String	
     , reqURL    :: String
     , reqArgs   :: [(String,String)]
     , reqBody   :: String
     }

data AjaxParser a = AjaxParser AjaxAPI
     		    	       (Request -> Either [AjaxParserFailure] a)

data AjaxParserFailure
     = NotCorrectMethod String String	-- what you found, what you expected
     | WrongCallName String String
     | ArgumentMissing String
     | DecodeError String String	-- what you found, what you expected 
     deriving Show

alts :: [AjaxParser a] -> AjaxParser a
alts as = AjaxParser (API_Alt (map (\ (AjaxParser a _ ) -> a) as))
     	  	     $ \ state -> 
		        let
			   res = map (\ (AjaxParser _ f) -> f state) as
			in 
			   case [ r | Right r <- res ] of
			     (ans:_) -> Right ans
			     [] -> Left $ concat [ e | Left e <- res ]
		       	 

withMethod :: String -> AjaxParser a -> AjaxParser a
withMethod method (AjaxParser apis fn) = AjaxParser apis fn -- debugging version
{-
withMethod method (AjaxParser apis fn) = AjaxParser apis $ \ req ->
	   if reqMethod req == method 
	   then fn req
	   else Left $ [NotCorrectMethod (reqMethod req) method]
-}	   

infixr 4 <*>

(<*>) :: AjaxParser a -> AjaxParser (a -> b) -> AjaxParser b
(<*>) (AjaxParser ys q) 
      (AjaxParser xs p) 
    = AjaxParser (API_Join xs ys) $ \ req ->
      case p req of
        Right f -> case q req of
	       	    Right a -> Right (f a)
		    Left msg -> Left msg
        Left msg -> Left msg

call :: String -> a -> AjaxParser a
call apiName caller = AjaxParser (API_Name apiName)
     	     	      $ \ req -> if reqURL req == ("/" ++ apiName)
		      	         then Right caller
			         else Left [WrongCallName (reqURL req) apiName]

argument :: AjaxTypeDict a -> String -> AjaxParser a
argument ty@(AjaxTypeDict _ _ tyName) str = res
   where
     res = AjaxParser (API_Arg str ty)
	                $ \ req -> 
			       case Data.List.lookup str (reqArgs req) of
			       	  Just val -> case decodeATD ty val of
				       	        Right a -> Right a
						Left r -> Left [ DecodeError str tyName ]
				  Nothing  -> Left [ ArgumentMissing $ show str ]

intArg    :: String -> AjaxParser Int
intArg    = argument intATD
boolArg    :: String -> AjaxParser Bool
boolArg   = argument boolATD
stringArg    :: String -> AjaxParser String
stringArg = argument stringATD
readArg    :: (Read a) => String -> AjaxParser a
readArg   = argument readATD

jsonArg    :: (JSON a) => String -> AjaxParser a
jsonArg   = argument jsonATD

------------------------------------------------------------------------------

getAllFuns :: AjaxParser a -> [AjaxAPI]
getAllFuns (AjaxParser apis _) = findAllFuns apis

findAllFuns :: AjaxAPI -> [AjaxAPI]
findAllFuns (API_Alt alts)   = concatMap findAllFuns alts
findAllFuns other            = [other]

ajaxURLCodeGen :: AjaxAPI -> String
ajaxURLCodeGen api = 
	       "// usage: " ++ fixName name ++ "(" ++ tyList ++ ")\n" ++
	       "function " ++ fixName name ++ "(" ++ argList ++ ") {\n" ++
	       "  return \"/" ++ name  ++ (if null callList 
	       	  	      	       	  then ""
	       	  	      	          else "?" ++ callList)
			      ++ "\";\n" ++
	       "}\n"	       		    
   where
     fixName = concatMap $ \ c -> case c of 
     	       		            '/' -> "_"
				    c   -> [c]
     callList = concat
     	      $ intersperse "&"
     	      	 [ arg ++ "=" ++ "\" + " ++ (encodeATD ty) arg ++ " + \""
	  	 | API_Arg arg ty <- findArgs api   
		 ]    

     tyList = concat 
     	     $ intersperse "," 
     	         [ nameATD ty
	  	 | API_Arg arg ty <- findArgs api   
		 ]
     argList = concat 
     	     $ intersperse "," 
     	         [ arg 
	  	 | API_Arg arg ty <- findArgs api   
		 ]

     Just name = findName api

     findName :: AjaxAPI -> Maybe String
     findName (API_Name str) = return str
     findName (API_Arg _ _)  = Nothing
     findName (API_Join a1 a2) = 
     	      case findName a1 of 
	        Nothing -> findName a2
		Just res -> return res

     findArgs :: AjaxAPI -> [AjaxAPI]
     findArgs (API_Name str)   = []
     findArgs (API_Arg arg ty) = [API_Arg arg ty]
     findArgs (API_Join a1 a2) = findArgs a1 ++ findArgs a2


ajaxFunCodeGen :: AjaxAPI -> String
ajaxFunCodeGen api = 
	       "// usage: " ++ fixName name ++ "(" ++ tyList ++ ")\n" ++
	       "function " ++ fixName name ++ "(" ++ argList ++ ") {\n" ++
	       "  send(\"/" ++ name  ++ "\",\"" ++ callList ++ "\");\n" ++
	       "}\n"	       		    
   where
     fixName = concatMap $ \ c -> case c of 
     	       		            '/' -> "_"
				    c   -> [c]

     callList = concat
     	      $ intersperse "&"
     	      	 [ arg ++ "=" ++ "\" + " ++ (encodeATD ty) arg ++ " + \""
	  	 | API_Arg arg ty <- findArgs api   
		 ]    

     tyList = concat 
     	     $ intersperse "," 
     	         [ nameATD ty
	  	 | API_Arg arg ty <- findArgs api   
		 ]
     argList = concat 
     	     $ intersperse "," 
     	         [ arg 
	  	 | API_Arg arg ty <- findArgs api   
		 ]

     Just name = findName api

     findName :: AjaxAPI -> Maybe String
     findName (API_Name str) = return str
     findName (API_Arg _ _)  = Nothing
     findName (API_Join a1 a2) = 
     	      case findName a1 of 
	        Nothing -> findName a2
		Just res -> return res

     findArgs :: AjaxAPI -> [AjaxAPI]
     findArgs (API_Name str)   = []
     findArgs (API_Arg arg ty) = [API_Arg arg ty]
     findArgs (API_Join a1 a2) = findArgs a1 ++ findArgs a2

------------------------------------------------------------------------------

intATD :: AjaxTypeDict Int
intATD =  AjaxTypeDict (\ a -> Right (read a))
       	  	       (\ a -> a)
		       "int"

boolATD :: AjaxTypeDict Bool
boolATD =  AjaxTypeDict (\ a -> if a == "true" then Right True
	   		   else if a == "false" then Right False
			   else Left ("error finding bool: " ++ show a))
       	  	        (\ a -> a)
		        "bool"

stringATD :: AjaxTypeDict String
stringATD =  AjaxTypeDict 
	     		(\ a -> Right a)
       	  	        (\ a -> "escape(" ++ a ++ ")")
		        "string"

readATD :: (Read a) => AjaxTypeDict a
readATD =  AjaxTypeDict 
	     		(\ a -> case reads a of
			          [(r,"")] -> Right $ r
				  _ -> Left $ "read error with : " ++ show a)
       	  	        (\ a -> "escape(" ++ a ++ ")")
		        "(Read a)"

jsonATD :: (JSON a) => AjaxTypeDict a
jsonATD =  AjaxTypeDict 
	     		(\ a -> case parse a of
			          Just json ->
				    case fromJSON json of
			              Just r -> Right $ r
				      _ -> Left $ "coerse error with : " ++ show json
			          _ -> Left $ "parse error with : " ++ show a)
       	  	        (\ a -> "escape(" ++ a ++ ".toJSONString())")
		        "(JSON a)"

------------------------------------------------------------------------------

type URLPath  = String
type ContentType = String

data PageResponse = PageResponse Int Bool ContentType String

--           :: [(String,String,AjaxAction PageResponse)]
--	    -> [(String,String,AjaxAction PageResponse)]

ajaxServer' :: Int	-- ^ port
	    -> (String -> [(String,String)] -> IO PageResponse)	-- POST
	    -> (String -> [(String,String)] -> IO PageResponse)	-- GET
	    -> IO ()
ajaxServer' portNum fn  = undefined

ajaxServer :: Int	-- ^ port
	   -> (AjaxParser (IO [AjaxCallback]))	-- POST, typically
	   -> (AjaxParser (IO PageResponse))	-- GET
	   -> (String -> IO PageResponse)	-- GET, ignore after ?...
	   -> IO ()
ajaxServer portNum rpcs pageRpcs getPage = do
--	   putStrLn $ ajax_code
	   server 8 portNum $ Server $ \ url args send -> do	
	      let req = Request "GET" url args ""
--	      print (url,args)
	      case url of
	      	   -- The magic page
	        "/ajax.js" -> 
		   respondWithPage (return (PageResponse
		   		    	        200
						False	-- for now
						"text/js"
						ajax_code
		   		    	    )) send
                _ -> case rpc_parser req of
		       Right fn -> do
		           callbacks <- fn
		       	   respondWithPage (return (PageResponse
							200
							False	-- always
							"text/js"
							(renderCallbacks callbacks)))
							send
		       Left msg ->  do
--		       	 print msg	-- for now
		         case pageRpc_parser req of
 	                    Right fn ->
		               respondWithPage fn send
		            Left _ ->
		               respondWithPage (getPage url) send
  where
	respondWithPage pageResFn send = do
	           PageResponse code cache ty body <- pageResFn
--		   print body
	           case code of
   	            200 -> reply send cache ty body
	            404 -> replyWithFailure send body

        pageRpc_parser = ajaxParser pageRpcs
        rpc_parser     = ajaxParser rpcs

	ajax_code = addr_gen ++ send_gen

	addr_gen = concatMap ajaxURLCodeGen $ getAllFuns pageRpcs
	send_gen = concatMap ajaxFunCodeGen $ getAllFuns rpcs

------------------------------------------------------------------------------

data AjaxCallback = AjaxCallback String [String]
     deriving Show

renderCallbacks :: [AjaxCallback] -> String
renderCallbacks = concatMap $ \ (AjaxCallback method args) -> 
	     method ++ "(" ++ concat (intersperse "," args) ++ ");\n"
ajaxParser :: AjaxParser a -> Request -> Either [AjaxParserFailure] a 
ajaxParser (AjaxParser _ f) req = f req

data AjaxArg      = AjaxArg { unAjaxArg :: String }

ajaxCallback  :: String -> AjaxCallback
ajaxCallback method = AjaxCallback method []

infixl 2 <@>

(<@>) :: (JSON arg) => AjaxCallback -> arg -> AjaxCallback
(<@>) (AjaxCallback method args) arg = 
     AjaxCallback method (args ++ [stringify $ toJSON arg])

class JSON a where
  toJSON :: a -> Value
  fromJSON :: Value -> Maybe a

instance JSON Value where
  toJSON = id
  fromJSON = return

instance JSON Bool where
  toJSON 		=Bool
  fromJSON (Bool b) = Just b
  fromJSON _            = Nothing

instance JSON Int where
  toJSON 		= Int
  fromJSON (Int i)  	= Just i
  fromJSON _            = Nothing

instance JSON Double where
  toJSON 		= Double
  fromJSON (Double i)  	= Just i
  fromJSON (Int i)  	= Just (fromInteger $ fromIntegral i)
  fromJSON _            = Nothing

instance JSON String where
  toJSON 		    =String
  fromJSON (String str) = Just str
  fromJSON _                = Nothing

instance JSON a => JSON [a] where
  toJSON 		    = Array . map toJSON
  fromJSON (Array arr)      = sequence [ fromJSON a | a <- arr ]
  fromJSON _                = Nothing

instance JSON a => JSON (Maybe a) where
  toJSON Nothing  = Object $ M.fromList [ ("tag",toJSON "Nothing")]
  toJSON (Just a) = Object $ M.fromList [ ("tag",toJSON "Just")
  	       	    	                , ("payload",toJSON a)
			                ]
  fromJSON (Object m) = case M.lookup "tag" m of
  	   	      	  Just (String "Nothing") -> Just Nothing
			  Just (String "Just")    -> do
			       v1 <- M.lookup "payload" m 
			       fromJSON v1
			  _    		          -> Nothing
  fromJSON _          = Nothing

altsJSON :: [Maybe a] -> Maybe a
altsJSON = listToMaybe . catMaybes 

findJSON :: (JSON a) => String -> JSON.Map String JSON.Value -> Maybe a
findJSON tagName obj = do
  val <- JSON.lookup tagName obj
  fromJSON val
