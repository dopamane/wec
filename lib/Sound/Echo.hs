module Sound.Echo (echo) where

import Data.List

echo
  :: Int      -- ^ delay sample count
  -> Int      -- ^ echo count
  -> Double   -- ^ volume decay [0,1]
  -> [Double] -- ^ input sound [0,1]
  -> [Double]
echo n c v s = map (clip . sum) $ transpose $
  (s ++ repeat 0) : [ replicate (n * i) 0 ++ map (a *) s ++ repeat 0
                    | (i, a) <- zip [1..c] $ iterate (* v) v]

clip :: Double -> Double
clip x
  | x > 0 = min   1  x
  | x < 0 = max (-1) x
  | otherwise = 0
