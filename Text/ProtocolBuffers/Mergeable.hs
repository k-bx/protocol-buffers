module Text.ProtocolBuffers.Mergeable(Mergeable(..),mayMerge) where
-- This is isomorphic to Monoid(mappend), but allows the derired instances without overlapping.
-- It also gives a default implementation to mergeAppend to take the second entry.

import Text.ProtocolBuffers.Basic
import Data.Monoid(mempty,mappend)

class Mergeable a where
  mergeEmpty :: a
  mergeEmpty = error "You did not define Mergeable.mergeEmpty!"
  mergeAppend :: a -> a -> a
  mergeAppend a b = b
  mergeConcat :: [a] -> a
  mergeConcat = foldr mergeAppend mergeEmpty

-- Base types are not very mergeable, but their Maybe type are:
instance Mergeable a => Mergeable (Maybe a) where mergeEmpty = Nothing; mergeAppend = mayMerge
instance Mergeable (Seq a) where mergeEmpty = mempty; mergeAppend = mappend
instance Mergeable Bool where mergeEmpty = False
instance Mergeable ByteString where mergeEmpty = mempty
instance Mergeable Double where mergeEmpty = 0.0
instance Mergeable Float where mergeEmpty = 0.0
instance Mergeable Int32 where mergeEmpty = 0
instance Mergeable Int64 where mergeEmpty = 0
instance Mergeable Word32 where mergeEmpty = 0
instance Mergeable Word64 where mergeEmpty = 0

{-# INLINE mayMerge #-}
mayMerge :: (Mergeable b) => Maybe b -> Maybe b -> Maybe b
mayMerge Nothing  y        = y
mayMerge x        Nothing  = x
mayMerge (Just x) (Just y) = Just (mergeAppend x y)
