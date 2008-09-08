module Text.ProtocolBuffers.Basic(Seq,Utf8(..),ByteString,Int32,Int64,Word32,Word64
                                 ,WireTag(..),FieldId(..),WireType(..),FieldType(..),EnumCode(..),WireSize
                                 ,Mergeable(..),Default(..),Wire(..)
--                                 ,Key(..),ExtField(..),ExtFieldValue,GPWitness(..),GPDyn(..)
--                                 ,ExtKey(..),GPB,ExtendMessage(..)
                                 ) where

import Data.Binary.Put(Put)
import Data.Bits(Bits)
import Data.ByteString.Lazy(ByteString)
import Data.Foldable as F(Foldable(foldl))
import Data.Generics(Data(..))
import Data.Int(Int32,Int64)
import Data.Ix(Ix)
import Data.Sequence(Seq)
import Data.Typeable(Typeable(..))
import Data.Word(Word32,Word64)
import Text.ProtocolBuffers.Get(Get)
-- Num instances are derived below to get fromInteger for case matching

newtype Utf8 = Utf8 {utf8 :: ByteString} deriving (Read,Show,Data,Typeable,Eq,Ord)

newtype WireTag = WireTag { getWireTag :: Word32 } -- bit concatenation of FieldId and WireType
  deriving (Eq,Ord,Read,Show,Num,Bits,Bounded,Data,Typeable)

newtype FieldId = FieldId { getFieldId :: Int32 } -- really 29 bits
  deriving (Eq,Ord,Read,Show,Num,Data,Typeable,Ix)

-- Note that valeus 19000-19999 are forbidden for FieldId
instance Bounded FieldId where
  minBound = 0
  maxBound = 536870911 -- 2^29-1

newtype WireType = WireType { getWireType :: Word32 }    -- really 3 bits
  deriving (Eq,Ord,Read,Show,Num,Data,Typeable)

instance Bounded WireType where
  minBound = 0
  maxBound = 7

newtype FieldType = FieldType { getFieldType :: Int } -- really [1..18] as fromEnum of Type from Type.hs
  deriving (Eq,Ord,Read,Show,Num,Data,Typeable)

instance Bounded FieldType where
  minBound = 1
  maxBound = 18

newtype EnumCode = EnumCode { getEnumCode :: Int32 }  -- really [0..maxBound::Int32] of some .proto defined enumeration
  deriving (Eq,Ord,Read,Show,Num,Data,Typeable) 

instance Bounded EnumCode where
  minBound = 0
  maxBound = 2147483647 -- 2^-31 -1 

type WireSize = Int64

-- | The 'Mergeable' class is not a Monoid, mergeEmpty is not a left
-- or right unit like mempty.  The default mergeAppend is to take the
-- second parameter and discard the first one.  The mergeConcat
-- defaults to foldl associativity.
class Mergeable a where
  mergeEmpty :: a
  mergeEmpty = error "You did not define Mergeable.mergeEmpty!"

  mergeAppend :: a -> a -> a
  mergeAppend _a b = b

  mergeConcat :: F.Foldable t => t a -> a
  mergeConcat = F.foldl mergeAppend mergeEmpty

-- | The Default class has the default-default values.  See
-- http://code.google.com/apis/protocolbuffers/docs/proto.html#optional
-- and also note that Enum defaultValue is the first one in the proto
-- file (there is always at least one value).
class Default a where
  defaultValue :: a

-- The first Int argument is fromEnum on
-- Text.DescriptorProtos.FieldDescriptorProto.Type.  The values of the
-- Int parameters cannot change without breaking all serialized
-- protocol buffers.
class Wire b where
  {-# INLINE wireSize #-}
  wireSize :: FieldType -> b -> WireSize
  {-# INLINE wirePut #-}
  wirePut :: FieldType -> b -> Put
  {-# INLINE wireGet #-}
  wireGet :: FieldType -> Get b
