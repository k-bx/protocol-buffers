module Text.ProtocolBuffers.Builder where

class Builder a where
  getBuildInfo :: a -> Int -> Maybe BuildInfo


