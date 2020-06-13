module CodeRenderActor where


import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Util

import Data.List

import Data.Maybe(fromJust)
import Reactive
import Common


codeRenderActor :: String -> String -> IO (String -> IO String)
codeRenderActor hpcDir srcDir = do
  mkActor [] $ 
    request $ \ file state -> do
	 (contents,state') 
	      <- case lookup file state of
	 	       Nothing -> do 
		             contents <- createHtml hpcDir srcDir file
			     return (contents,(file,contents) : state)
		       Just contents -> do
			     return (contents,state)

         let wrap = "<html><HEAD>" ++
	     	    "<LINK REL =\"stylesheet\" HREF =\"default.css\" TYPE =\"text/css\"></LINK></HEAD>" ++
	     	     "<body>" ++ contents ++ 
	            "<script type=\"text/javascript\">\n" ++
		    "parent.status.codeloaded( " ++ show file ++ ");\n" ++
		    "</script>\n" ++
		    "</body></html>"
         return (wrap,state')


------------------------------------------------------------------------------

createHtml :: String -> String -> String -> IO String
createHtml hpcDir srcDir modName = do
  
  mix@(Mix origFile _ _ tabStop mix') <- readMix [srcDir ++ "/" ++ hpcDir] (Left modName)

--  print mix

  let info = [ (pos,id)
       	     | (id,(pos,boxLabel)) <- zip [0 ..] mix'
{-
	     , case boxLabel of
	       	  ExpBox {} -> True
	       	  AltBox {} -> True
	          TopLevelBox {} -> True
	          LocalBox {} -> True
		  _ -> False
-}
             ]

  let tickLocs = [ (Loc ln1 c1,Loc ln2 c2,id)
    	         | (pos,id) <- info
	         , let (ln1,c1,ln2,c2) = fromHpcPos pos
	         ]

  let sortedTickLocs = sortBy (\ (locA1,locZ1,_) (locA2,locZ2,_) ->
                              (locA1,locZ2) `compare` (locA2,locZ1)) tickLocs

--  print sortedTickLocs

  content <- readFileFromPath origFile srcDir

  let content' = addMarkup 1 content (Loc 1 1) [] sortedTickLocs
  let show' = reverse . take 5 . (++ "       ") . reverse . show
  let addLine n xs = "<span class=\"lineno\">" ++ show' n ++ " </span>" ++ xs 
  let addLines = unlines . map (uncurry addLine) . zip [1..] . lines
  return ("<pre>\n" ++ addLines content' ++ "\n</pre>\n")

type Markup = Int

addMarkup :: Int		-- tabStop
	  -> String		-- text to mark up
	  -> Loc		-- current location
	  -> [(Loc,Markup)]	-- stack of open ticks, with closing location
	  -> [(Loc,Loc,Markup)]	-- sorted list of tick location pairs
	  -> String

-- check the pre-condition.
--addMarkup tabStop cs loc os ticks 
--   | not (isSorted (map fst os)) = error $ "addMarkup: bad closing ordering: " ++ show os

--addMarkup tabStop cs loc os@(_:_) ticks 
--   | trace (show (loc,os,take 10 ticks)) False = undefined

-- close all open ticks, if we have reached the end
addMarkup _ [] loc os [] =
  concatMap (const closeTick) os 
addMarkup tabStop cs loc ((o,_):os) ticks | loc > o =
  closeTick ++ addMarkup tabStop cs loc os ticks

--addMarkup tabStop cs loc os ((t1,t2,tik@(TopLevelDecl {})):ticks) | loc == t1 =
--   openTick tik ++ closeTick ++ addMarkup tabStop cs loc os ticks

addMarkup tabStop cs loc os ((t1,t2,tik):ticks) | loc == t1 =
  openTick tik ++ addMarkup tabStop cs loc (addTo (t2,tik) os) ticks
  where

  addTo (t,tik) []             = [(t,tik)]
  addTo (t,tik) ((t',tik'):xs) | t <= t' = (t,tik):(t',tik'):xs
  	 	 	       | t > t'  = (t',tik):(t',tik'):xs 

addMarkup tabStop cs loc os ((t1,t2,tik):ticks) | loc > t1 =
	  -- throw away this tick, because it is from a previous place ??
	  addMarkup tabStop cs loc os ticks

addMarkup tabStop ('\n':cs) loc@(Loc ln col) os@((Loc ln2 col2,_):_) ticks 
	  | ln == ln2 && col < col2
	  = addMarkup tabStop (' ':'\n':cs) loc os ticks 
addMarkup tabStop (c:cs) loc@(Loc _ p) os ticks =
  if c=='\n' && os/=[] then
    c : "<span class=\"spaces\">" ++ expand 1 w ++ "</span>" ++
    addMarkup tabStop cs' loc' os ticks
  else if c=='\t' then
    expand p "\t" ++ addMarkup tabStop cs (incBy c loc) os ticks
  else
    escape c ++ addMarkup tabStop cs (incBy c loc) os ticks
  where
  (w,cs') = span (`elem` " \t") cs
  loc' = foldl (flip incBy) loc (c:w)
  escape '>' = "&gt;"
  escape '<' = "&lt;"
  escape '"' = "&quot;"
  escape '&' = "&amp;"
  escape c  = [c]

  expand :: Int -> String -> String
  expand _ ""       = "" 
  expand c ('\t':s) = replicate (c' - c) ' ' ++ expand c' s
    where
    c' = tabStopAfter 8 c
  expand c (' ':s)  = ' ' : expand (c+1) s
  expand _ _        = error "bad character in string for expansion"
  
  incBy :: Char -> Loc -> Loc
  incBy '\n' (Loc ln c) = Loc (succ ln) 1
  incBy '\t' (Loc ln c) = Loc ln (tabStopAfter tabStop c)
  incBy _    (Loc ln c) = Loc ln (succ c)
  
  tabStopAfter :: Int -> Int -> Int
  tabStopAfter tabStop c = fromJust (find (>c) [1,(tabStop + 1)..])
  
addMarkup tabStop cs loc os ticks = 
	  "ERROR: " ++ show (take 10 cs,tabStop,loc,take 10 os,take 10 ticks)

data Loc = Loc !Int !Int
     	 deriving (Eq,Ord,Show)

openTick :: Markup -> String
openTick n = "<span class=\"tick\" id=\"t_" ++ show n ++ "\">" 

closeTick = "</span>"



readFileFromPath :: String -> String -> IO String
readFileFromPath filename@('/':_) _ = readFile filename
readFileFromPath filename path = readTheFile (splitPath path)
  where
	splitPath path = case span (/= ':') path of
			   (dir,':':more) -> dir : splitPath more
			   (dir,[])       -> [dir]

	readTheFile :: [String] -> IO String
	readTheFile [] = error $ "could not find " ++ show filename 
			         ++ " in path " ++ show path
	readTheFile (dir:dirs) = 
		Prelude.catch 
	              (do str <- readFile (dir ++ "/" ++ filename) 
			  return str) 
		      (\ _ -> readTheFile dirs)

------------------------------------------------------------------------------
