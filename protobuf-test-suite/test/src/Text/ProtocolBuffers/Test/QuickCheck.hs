{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.ProtocolBuffers.Test.QuickCheck
  ( module Test.QuickCheck
  , quickCheckTests
  )
  where

import Test.QuickCheck
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header(UnknownField)
import Test.Tasty.QuickCheck as QC
import Test.Tasty (TestTree, testGroup)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Aeson as J
import Data.Proxy
import System.Exit
import System.Process.ByteString.Lazy
import Text.ProtocolBuffers (messagePutText, messageGetText, messagePut, messageGet, ReflectDescriptor, Wire)
import Text.ProtocolBuffers.TextMessage (TextMsg)

instance Arbitrary UnknownField where
  arbitrary = pure defaultValue

instance Arbitrary Utf8 where
  arbitrary = do
    len <- frequency
             [ (3, choose (1,3))
             , (1, return 0) ]
    fmap (Utf8 . U.fromString) (vector len)
  shrink = fmap (Utf8 . U.fromString) . shrink . U.toString . utf8

instance Arbitrary L.ByteString where
  arbitrary = do
    len <- frequency
             [ (3, choose (1,3))
             , (1, return 0) ]
    fmap L.pack (vector len)

quickCheckTests :: forall a. (Arbitrary a, Show a, ReflectDescriptor a, Wire a, Eq a, TextMsg a, J.ToJSON a, J.FromJSON a) => String -> String -> Proxy a -> TestTree
quickCheckTests name messageName proxy = testGroup (name ++ " QuickChecks")
  [ QC.testProperty "wire-encoded then decoded identity" $ roundTripWireEncodeDecode proxy
  , QC.testProperty "json-encoded then decoded identity" $ roundTripJsonEncodeDecode proxy
  , QC.testProperty "text-encoded and decoded via protoc identity" $ textEncodeWireDecode proxy messageName
  , QC.testProperty "wire-encoded and decoded via protoc identity" $ wireEncodeTextDecode proxy messageName
  ]

roundTripWireEncodeDecode :: (ReflectDescriptor a, Wire a, Eq a, Show a) => Proxy a -> a -> Property
roundTripWireEncodeDecode _ x =
  let encoded = messagePut x
  in checkWireEncoded encoded x

checkWireEncoded :: (ReflectDescriptor a, Wire a, Eq a, Show a) => L.ByteString -> a -> Property
checkWireEncoded encoded x =
  case messageGet encoded of
    Right (result, "") -> x === result
    Left e -> counterexample e False

roundTripJsonEncodeDecode :: (J.ToJSON a, J.FromJSON a, Eq a, Show a) => Proxy a -> a -> Property
roundTripJsonEncodeDecode _ x =
  let encoded = J.toJSON x
  in case J.fromJSON encoded of
       J.Success result -> x === result
       J.Error e -> counterexample e False

textEncodeWireDecode :: (TextMsg a, Wire a, ReflectDescriptor a, Eq a, Show a) => Proxy a -> String -> a -> Property
textEncodeWireDecode _ messageName x =
  ioProperty $ do
    let textIn = messagePutText x
    (exitCode, stdout, stderr) <-
      readProcessWithExitCode
      "protoc"
      [ "--encode", messageName
      , "proto/mymap.proto"
      , "proto/films.proto"
      , "proto/school.proto"
      ]
      (U.fromString textIn)
    case exitCode of
      ExitSuccess -> do
        return $ checkWireEncoded stdout x
      ExitFailure i ->
        return $ counterexample (LB.unpack stderr) False

textEncodeUsingProtoc :: (Wire a, ReflectDescriptor a, Eq a, Show a) => String -> a -> IO (ExitCode, String, String)
textEncodeUsingProtoc messageName x = do
    (exitCode, stdout, stderr) <-
      readProcessWithExitCode
      "protoc"
      [ "--decode", messageName
      , "proto/mymap.proto"
      , "proto/films.proto"
      , "proto/school.proto"
      ]
      (messagePut x)
    return (exitCode, U.toString stdout, U.toString stderr)

wireEncodeTextDecode :: (TextMsg a, Wire a, ReflectDescriptor a, Eq a, Show a) => Proxy a -> String -> a -> Property
wireEncodeTextDecode _ messageName x =
  ioProperty $ do
    (exitCode, stdout, stderr) <- textEncodeUsingProtoc messageName x
    return $
      case exitCode of
        ExitSuccess -> do
          case messageGetText stdout of
            Left e -> counterexample ("=== OUTPUT ===\n" <> stdout <> "\n=== ERROR ===\n" <> e) False
            Right decoded -> counterexample stdout $ x === decoded
        ExitFailure i ->
          counterexample stderr False
