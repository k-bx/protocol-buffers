module Text.DescriptorProtos.EnumDescriptorProto
  (EnumDescriptorProto(..))
 where

import Text.ProtocolBuffers.Header

import qualified Text.DescriptorProtos.EnumValueDescriptorProto as DescriptorProtos(EnumValueDescriptorProto)
import qualified Text.DescriptorProtos.EnumOptions as DescriptorProtos(EnumOptions)

data EnumDescriptorProto = EnumDescriptorProto
    { name :: Maybe ByteString
    , value :: Seq DescriptorProtos.EnumValueDescriptorProto
    , options :: Maybe DescriptorProtos.EnumOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''EnumDescriptorProto )

instance Default EnumDescriptorProto where
