module Utils where

showWithCommas :: Integer -> String
showWithCommas n = showBigNum n
  where showBigNum n | n <= 9999 = show n
		     | otherwise = showBigNum' (n `div` 1000) ++ "," ++ showWith (n `mod` 1000)
        showBigNum' n | n <= 999 = show n
		      | otherwise = showBigNum' (n `div` 1000) ++ "," ++ showWith (n `mod` 1000)
        showWith n = take 3 $ (take (3 - length (show n)) "000" ++) $ show n

