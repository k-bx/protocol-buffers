module Text.ProtocolBuffers.Default(Default(..)) where

import Text.ProtocolBuffers.Mergeable

class Mergeable a => Default a where
  defaultValue :: a
  defaultValue = mergeEmpty
