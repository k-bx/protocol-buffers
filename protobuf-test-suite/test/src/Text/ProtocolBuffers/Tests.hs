module Text.ProtocolBuffers.Tests
  ( tests
  )
  where

import Test.HUnit (Test(..), runTestTT, Counts)

import Text.ProtocolBuffers.Tests.AddressBook (addressBookTest)

tests :: IO Counts
tests = runTestTT $ TestList
  [ TestLabel "Address Book test" addressBookTest
  ]
