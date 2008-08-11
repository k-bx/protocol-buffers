module Text.ProtocolBuffers.Mergeable(Mergeable(..),mayMerge) where
-- This types are isomorphic to Monoid(mappend).  But the sematics are
-- different, since mergeEmpty is likely only a left idenitity and not
-- a right identity. The Mergeable class also has a default
-- implementation to mergeAppend that take the second parameter.

import Text.ProtocolBuffers.Basic
import qualified Data.Foldable as F(Foldable(foldr))
import Data.Monoid(mempty,mappend)

class Mergeable a where
  mergeEmpty :: a
-- mergeEmpty = error "You did not define Mergeable.mergeEmpty!"

  mergeAppend :: a -> a -> a
  mergeAppend a b = b

  mergeConcat :: F.Foldable t => t a -> a
  mergeConcat = F.foldr mergeAppend mergeEmpty

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
