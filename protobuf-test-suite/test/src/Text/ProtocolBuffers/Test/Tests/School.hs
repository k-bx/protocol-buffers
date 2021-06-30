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
schoolQuickChecks = quickCheckTests "School" (Proxy :: Proxy Dormitory)

instance Arbitrary Admin where
  arbitrary = Admin <$> liftA uFromString arbitrary
                    <*> pure defaultValue

instance Arbitrary Faculty where
  arbitrary = Faculty <$> liftA uFromString arbitrary
                      <*> oneof [ liftA (Just . uFromString) arbitrary
                                , pure Nothing
                                ]
                      <*> liftA Seq.fromList (listOf arbitraryUtf8)
                      <*> pure defaultValue
    where
      arbitraryUtf8 :: Gen Utf8
      arbitraryUtf8 = liftA uFromString arbitrary

instance Arbitrary Student where
  arbitrary = Student <$> arbitrary
                      <*> oneof [ liftA (Just . uFromString) arbitrary
                                , pure Nothing
                                ]
                      <*> pure defaultValue

instance Arbitrary Member where
  arbitrary = Member <$> arbitrary
                     <*> liftA uFromString arbitrary
                     <*> frequency [ (3, liftA Just arbitraryProperty)
                                   , (1, pure Nothing)
                                   ]
                     <*> pure defaultValue
    where
      arbitraryProperty :: Gen Property'.Property
      arbitraryProperty = oneof [ liftA Prop_student arbitrary
                                , liftA Prop_faculty arbitrary
                                , liftA Prop_admin arbitrary
                                ]

instance Arbitrary Dormitory where
  arbitrary = Dormitory <$> liftA uFromString arbitrary
                        <*> liftA Seq.fromList (listOf arbitrary)
                        <*> pure defaultValue

