{-# LANGUAGE OverloadedStrings #-}
module Text.ProtocolBuffers.Test.Tests.School
  ( schoolQuickChecks
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck ()

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

import qualified HSCodeGen.School.Dormitory      as Dormitory'
import qualified HSCodeGen.School.Member         as Member'
import qualified HSCodeGen.School.Member.Admin   as Admin'
import qualified HSCodeGen.School.Member.Faculty as Faculty'
import qualified HSCodeGen.School.Member.Student as Student'
import qualified HSCodeGen.School.Member.Property as Property'

import HSCodeGen.School.Dormitory       (Dormitory (..))
import HSCodeGen.School.Member          (Member (..))
import HSCodeGen.School.Member.Admin    (Admin (..))
import HSCodeGen.School.Member.Faculty  (Faculty (..))
import HSCodeGen.School.Member.Student  (Student (..))
import HSCodeGen.School.Member.Property (Property (..))
import Text.ProtocolBuffers.Test.QuickCheck (quickCheckTests)

schoolQuickChecks :: TestTree
schoolQuickChecks = quickCheckTests "School" "dormitory" (Proxy :: Proxy Dormitory)

instance Arbitrary Admin where
  shrink = genericShrink
  arbitrary = Admin <$> arbitrary
                    <*> arbitrary

instance Arbitrary Faculty where
  shrink = genericShrink
  arbitrary = Faculty <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

instance Arbitrary Student where
  shrink = genericShrink
  arbitrary = Student <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary

instance Arbitrary Member where
  shrink = genericShrink
  arbitrary = Member <$> arbitrary
                     <*> arbitrary
                     <*> frequency [ (3, liftA Just arbitrary)
                                   , (1, pure Nothing)
                                   ]
                     <*> arbitrary

instance Arbitrary Property'.Property where
  shrink = genericShrink
  arbitrary =
    oneof [ liftA Prop_student arbitrary
          , liftA Prop_faculty arbitrary
          , liftA Prop_admin arbitrary
          ]

instance Arbitrary Dormitory where
  shrink = genericShrink
  arbitrary = Dormitory <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary

