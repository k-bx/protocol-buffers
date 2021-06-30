{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Text.ProtocolBuffers.Test.Tests.Map
  ( mapQuickChecks
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import qualified Data.Aeson as J
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Applicative (liftA)

import Data.Proxy

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.TextMessage
import Text.ProtocolBuffers.WireMessage

import HSCodeGen.Mymap.WrappedMap
import HSCodeGen.Mymap.WithMap
import HSCodeGen.Mymap.Value_type

import Text.ProtocolBuffers.Test.QuickCheck (quickCheckTests)

mapQuickChecks :: TestTree
mapQuickChecks = quickCheckTests "Map" (Proxy :: Proxy WrappedMap)

instance Arbitrary WrappedMap where
  arbitrary =
    WrappedMap
      <$> arbitrary
      <*> arbitrary

instance Arbitrary WithMap where
  arbitrary =
    WithMap
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Value_type where
  arbitrary =
    Value_type
      <$> arbitrary
      <*> arbitrary
