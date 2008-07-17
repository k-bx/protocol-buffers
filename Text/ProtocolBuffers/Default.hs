module Text.ProtocolBuffers.Default(Default(..)) where

import Text.ProtocolBuffers.Mergeable (Mergeable(mergeEmpty))

-- Anything with an "mempty / mergeEmpty" can be a trivial "Mondad / Mergeable"
--
-- So it makes some sense to make the default defaultValue be this empty value
class Mergeable a => Default a where
  defaultValue :: a
  defaultValue = mergeEmpty
