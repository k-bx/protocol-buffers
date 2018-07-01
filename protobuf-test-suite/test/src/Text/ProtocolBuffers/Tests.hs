module Text.ProtocolBuffers.Tests
  ( tests
  )
  where

import Test.Tasty (defaultMain, testGroup)

import Text.ProtocolBuffers.Tests.AddressBook (addressBookTests)

tests :: IO ()
tests = defaultMain $ testGroup "Protocol Buffers tests" $
  [ addressBookTests
  ]
