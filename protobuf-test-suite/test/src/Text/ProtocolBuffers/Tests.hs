module Text.ProtocolBuffers.Tests
  ( tests
  )
  where

import Test.Tasty (defaultMain, testGroup)

import Text.ProtocolBuffers.Tests.AddressBook (addressBookTests, addressBookQuickChecks)
import Text.ProtocolBuffers.Tests.School (schoolQuickChecks)

tests :: IO ()
tests = defaultMain $ testGroup "Protocol Buffers tests" $
  [ addressBookTests
  , addressBookQuickChecks
  , schoolQuickChecks
  ]
