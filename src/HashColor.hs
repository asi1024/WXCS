module HashColor (
  hueColor,
  cssColor
  ) where

import Data.Hashable

maxHue :: Int
maxHue = 65536

hueColor :: String -> Int
hueColor s = (hash s) `mod` maxHue

cssColor :: String -> String
cssColor s = "hsl(" ++ show ((hueColor s) * 360 `div` maxHue) ++ ",100%,50%)"

