-- This is isomorphic to Monoid(mappend), but allows the derired instances without overlapping.
-- It also gives a default implementation to mergeAppend to take the second entry.
module Text.ProtocolBuffers.Mergeable(Mergeable(..),mayMerge) where

import Data.ByteString(ByteString)
import Data.Sequence(Seq)
import Data.Monoid(mempty,mappend)
import Data.Int(Int32,Int64)
import Data.Word(Word32,Word64)

class Mergeable a where
  mergeEmpty :: a
  mergeEmpty = error "You did not define Mergeable.mergeEmpty!"
  mergeAppend :: a -> a -> a
  mergeAppend a b = b
  mergeConcat :: [a] -> a
  mergeConcat = foldr mergeAppend mergeEmpty

-- Base types are not very mergeable, but their Maybe type are:
instance Mergeable Bool where
instance Mergeable Int32 where
instance Mergeable Int64 where
instance Mergeable Word32 where
instance Mergeable Word64 where
instance Mergeable ByteString where
instance Mergeable (Maybe Bool) where mergeEmpty = Nothing; mergeAppend = mayMerge
instance Mergeable (Maybe Int32) where mergeEmpty = Nothing; mergeAppend = mayMerge
instance Mergeable (Maybe Int64) where mergeEmpty = Nothing; mergeAppend = mayMerge
instance Mergeable (Maybe Word32) where mergeEmpty = Nothing; mergeAppend = mayMerge
instance Mergeable (Maybe Word64) where mergeEmpty = Nothing; mergeAppend = mayMerge
instance Mergeable (Maybe ByteString) where mergeEmpty = Nothing; mergeAppend = mayMerge
instance Mergeable (Seq a) where mergeEmpty = mempty; mergeAppend = mappend

{-# INLINE mayMerge #-}
mayMerge :: (Mergeable b) => Maybe b -> Maybe b -> Maybe b
mayMerge Nothing y = y
mayMerge x Nothing = x
mayMerge (Just x) (Just y) = Just (mergeAppend x y)

{-# INLINE mayLast #-}
mayLast :: Maybe b -> Maybe b -> Maybe b
mayLast Nothing y = y
mayLast x Nothing = x
mayLast _ y = y
