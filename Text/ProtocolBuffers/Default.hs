module Text.ProtocolBuffers.Default(Default(..)) where

import Text.ProtocolBuffers.Basic
import Data.Monoid(mempty)

instance Default a => Default (Maybe a) where defaultValue = Just defaultValue
instance Default (Seq a) where defaultValue = mempty
instance Default Bool where defaultValue = False
instance Default ByteString where defaultValue = mempty
instance Default Utf8 where defaultValue = Utf8 mempty
instance Default Double where defaultValue = 0
instance Default Float where defaultValue = 0
instance Default Int32 where defaultValue = 0
instance Default Int64 where defaultValue = 0
instance Default Word32 where defaultValue = 0
instance Default Word64 where defaultValue = 0
