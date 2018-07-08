module Text.ProtocolBuffers.Test.QuickCheck
  ( module Test.QuickCheck
  )
  where

import Test.QuickCheck
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header(UnknownField)

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

-- instance Arbitrary a => Arbitrary (Seq a) where
--   arbitrary = do
--     len <- frequency
--              [ (3, choose (1,3))
--              , (1, return 0) ]
--     fmap Seq.fromList (vector len)
