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

instance Arbitrary L.ByteString where
  arbitrary = do
    len <- frequency
             [ (3, choose (1,3))
             , (1, return 0) ]
    fmap L.pack (vector len)

quickCheckTests :: forall a. (Arbitrary a, Show a, ReflectDescriptor a, Wire a, Eq a, TextMsg a, J.ToJSON a, J.FromJSON a) => String -> Proxy a -> TestTree
quickCheckTests name proxy = testGroup (name <> " QuickChecks")
  [ QC.testProperty "wire-encoded then decoded identity" $ roundTripWireEncodeDecode proxy
  , QC.testProperty "json-encoded then decoded identity" $ roundTripJsonEncodeDecode proxy
  ]

roundTripWireEncodeDecode :: (ReflectDescriptor a, Wire a, Eq a, Show a) => Proxy a -> a -> Property
roundTripWireEncodeDecode _ x =
  let encoded = messagePut x
  in case messageGet encoded of
       Right (result, "") -> x === result
       Left e -> Right x === Left e

roundTripJsonEncodeDecode :: (J.ToJSON a, J.FromJSON a, Eq a, Show a) => Proxy a -> a -> Property
roundTripJsonEncodeDecode _ x =
  let encoded = J.toJSON x
  in case J.fromJSON encoded of
       J.Success result -> x === result
       J.Error e -> Right x === Left e
