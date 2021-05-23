{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Text.ProtocolBuffers.Test.Tests.Grpc
  ( grpcTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck ()

import qualified Data.Aeson as J
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Applicative (liftA)

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.TextMessage
import Text.ProtocolBuffers.WireMessage

import HSCodeGen.Grpc ()

grpcTests :: TestTree
grpcTests = testGroup "GRPC tests" []
