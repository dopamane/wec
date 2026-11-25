module Main (main) where

import Sound.Echo
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Sound.Echo"
  [ testCase "empty" $ echo 0 0 0 [] @?= []
  ]
