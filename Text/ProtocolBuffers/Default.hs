module Text.ProtocolBuffers.Default(Default(..)) where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Mergeable (Mergeable(mergeEmpty))

instance Default a => Default (Maybe a) where defaultValue = Just defaultValue

-- Take the mergeEmpty as the defaultValue
instance Default (Seq a) where
instance Default Bool where
instance Default ByteString where
instance Default Utf8 where
instance Default Double where
instance Default Float where
instance Default Int32 where
instance Default Int64 where
instance Default Word32 where
instance Default Word64 where
