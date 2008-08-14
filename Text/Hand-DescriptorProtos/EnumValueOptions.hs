{-# LANGUAGE TemplateHaskell #-}

module Text.DescriptorProtos.EnumValueOptions
  (EnumValueOptions(..))
 where

import Text.ProtocolBuffers.Header

data EnumValueOptions = EnumValueOptions
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''EnumValueOptions )

instance Default EnumValueOptions where
