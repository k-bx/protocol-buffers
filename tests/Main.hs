module Main where

import Test.QuickCheck
import Arb.UnittestProto

main =  do
  mapM_ (\(name,test) -> putStrLn name >> quickCheck test) tests_TestAllTypes
  mapM_ (\(name,test) -> putStrLn name >> quickCheck test) tests_TestAllExtensions
