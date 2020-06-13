module Flags where


-- Where to get the debugging stream from
data Mode
     = ExecMode String [String]
     | FileMode String
     | AddrMode String Int

data Flags = Flags
     { mode :: Mode 
     , hpcdir :: String
     , srcdir :: String
     , port :: Int	-- ^which port to provide HTTP on for Ajax viewer
     , woop :: Int	-- ^window of opertunity
     }


-- Hack for now

parseFlags :: [String] -> Flags
parseFlags ("--port":num:args) 
	            = (parseFlags args)
		    { port = read num }
parseFlags ("--hpcdir":dir:args) 
	            = (parseFlags args)
		    { hpcdir = dir }
parseFlags ("--srcdir":dir:args) 
	            = (parseFlags args)
		    { srcdir = dir }
parseFlags (arg:args) = Flags (ExecMode arg args)
		      	      (".hpc")
			      (".")
	  	       	      8091
			      10000



