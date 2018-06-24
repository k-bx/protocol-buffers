module Main where

import Test.HUnit (Counts)

import Text.ProtocolBuffers.Tests (tests)

main :: IO Counts
main = tests
