-- | This exposes the client API
module Text.ProtocolBuffers where

import Text.ProtocolBuffers.Basic(Seq,Utf8(..),ByteString,Int32,Int64,Word32,Word64
                                 ,WireTag,FieldId,WireType,FieldType,EnumCode,WireSize
                                 ,Mergeable(..),Default(..),Wire)
import Text.ProtocolBuffers.Default()
import Text.ProtocolBuffers.Extensions(Key,ExtKey(getExt,putExt,clearExt),MessageAPI(..))
import Text.ProtocolBuffers.Mergeable()
import Text.ProtocolBuffers.Reflections(ReflectDescriptor(..),ReflectEnum(..),ProtoName(..),HsDefault(..),EnumInfoApp
                                       ,KeyInfo,FieldInfo(..),DescriptorInfo(..),EnumInfo(..),ProtoInfo(..))
import Text.ProtocolBuffers.WireMessage(Put,Get,runPut,runGet,runGetOnLazy
                                       ,messageSize,messagePut,messageGet,messagePutM,messageGetM
                                       ,bareMessageSize,bareMessagePut,bareMessageGet,bareMessagePutM,bareMessageGetM)
