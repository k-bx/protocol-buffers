module ProtocolBuffers.Default(Default(..)) where

import Data.Monoid

class Monoid a => Default a where
  defaultValue :: a
  defaultValue = mempty
