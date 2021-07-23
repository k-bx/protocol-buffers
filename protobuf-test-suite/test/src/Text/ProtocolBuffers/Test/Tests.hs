module Text.ProtocolBuffers.Test.Tests
  ( tests
  )
  where

import Test.Tasty (defaultMain, testGroup)

import Text.ProtocolBuffers.Test.Tests.AddressBook (addressBookTests, addressBookQuickChecks)
import Text.ProtocolBuffers.Test.Tests.School (schoolQuickChecks)
import Text.ProtocolBuffers.Test.Tests.Films (playlistQuickChecks)
import Text.ProtocolBuffers.Test.Tests.Grpc (grpcTests)
import Text.ProtocolBuffers.Test.Tests.Map (mapQuickChecks)

tests :: IO ()
tests = defaultMain $ testGroup "Protocol Buffers tests" $
  [ addressBookTests
  , addressBookQuickChecks
  , schoolQuickChecks
  , playlistQuickChecks
  , mapQuickChecks
  , grpcTests
  ]
