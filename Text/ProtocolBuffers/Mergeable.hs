-- | This provides instance of 'Mergeable' for the basic field types.
module Text.ProtocolBuffers.Mergeable() where

import Text.ProtocolBuffers.Basic
-- import Data.Monoid(mempty,mappend)
import Data.Sequence(empty,(><))

-- Base types are not very mergeable, but their Maybe and Seq versions are:
instance Mergeable a => Mergeable (Maybe a) where
    mergeEmpty = Nothing
    mergeAppend = mayMerge

instance Mergeable (Seq a) where
    mergeEmpty = empty
    mergeAppend = (><)

-- These all have errors as mergeEmpty and use the second paramater for mergeAppend
instance Mergeable Bool
instance Mergeable Utf8
instance Mergeable ByteString
instance Mergeable Double
instance Mergeable Float
instance Mergeable Int32
instance Mergeable Int64
instance Mergeable Word32
instance Mergeable Word64

{-# INLINE mayMerge #-}
mayMerge :: (Mergeable b) => Maybe b -> Maybe b -> Maybe b
mayMerge Nothing  y        = y
mayMerge x        Nothing  = x
mayMerge (Just x) (Just y) = Just (mergeAppend x y)
