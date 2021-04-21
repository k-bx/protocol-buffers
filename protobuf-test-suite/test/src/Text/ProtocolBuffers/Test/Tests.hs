module Text.ProtocolBuffers.Test.Tests
  ( tests
  )
  where

import Test.Tasty (defaultMain, testGroup)

import Text.ProtocolBuffers.Test.Tests.AddressBook (addressBookTests, addressBookQuickChecks)
import Text.ProtocolBuffers.Test.Tests.School (schoolQuickChecks)
import Text.ProtocolBuffers.Test.Tests.Films (playlistQuickChecks)
import Text.ProtocolBuffers.Test.Tests.Grpc (grpcTests)

tests :: IO ()
tests = defaultMain $ testGroup "Protocol Buffers tests" $
  [ addressBookTests
  , addressBookQuickChecks
  , schoolQuickChecks
  , playlistQuickChecks
  ]
