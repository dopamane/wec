module Main (main) where

import Sound.WEC

main :: IO ()
main = do
  putStrLn $ "Hello, Haskell!"
  print $ wec 5 []
