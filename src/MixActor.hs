module MixActor where

import Trace.Hpc.Mix
import Reactive

-- Cache the Mix file inside here.
mixActor :: String -> String -> IO (String -> IO Mix)
mixActor hpcDir srcDir = return $
	 \ file -> readMix [srcDir ++ "/" ++ hpcDir] (Left file)
