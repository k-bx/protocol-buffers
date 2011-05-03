module Main where

import Test.QuickCheck
import Arb.UnittestProto

main =  do
  cs
  mapM_ (\(name,test) -> putStrLn name >> quickCheck test) tests_TestAllExtensions
  mapM_ (\(name,test) -> putStrLn name >> quickCheck test) tests_TestAllTypes

