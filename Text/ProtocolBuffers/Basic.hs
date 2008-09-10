-- | "Text.ProtocolBuffers.Basic" defines or re-exports all the basic field types.
-- It also defined the Mergeable, Default, and Wire classes
module Text.ProtocolBuffers.Basic(Seq,Utf8(..),ByteString,Int32,Int64,Word32,Word64
                                 ,WireTag(..),FieldId(..),WireType(..),FieldType(..),EnumCode(..),WireSize
                                 ,Mergeable(..),Default(..),Wire(..)
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

-- Num instances are derived below for the purpose of getting fromInteger for case matching

-- | 'Utf8' is used to mark 'ByteString' values that (should) contain
-- valud utf8 encoded strings.  This type is used to represent
-- 'TYPE_STRING' values.
newtype Utf8 = Utf8 {utf8 :: ByteString} deriving (Read,Show,Data,Typeable,Eq,Ord)

-- | 'WireTag' is the 32 bit value with the upper 29 bits being the
-- 'FieldId' and the lower 3 bits being the 'WireType'
newtype WireTag = WireTag { getWireTag :: Word32 } -- bit concatenation of FieldId and WireType
  deriving (Eq,Ord,Read,Show,Num,Bits,Bounded,Data,Typeable)

-- | 'FieldId' is the field number which can be in the range 1 to
-- 2^29-1 but the value from 19000 to 19999 are forbidden (so sayeth
-- Google).
newtype FieldId = FieldId { getFieldId :: Int32 } -- really 29 bits
  deriving (Eq,Ord,Read,Show,Num,Data,Typeable,Ix)

-- Note that valeus 19000-19999 are forbidden for FieldId
instance Bounded FieldId where
  minBound = 0
  maxBound = 536870911 -- 2^29-1

-- | 'WireType' is the 3 bit wire encoding value, and is currently in
-- | the range 1 to 5.
newtype WireType = WireType { getWireType :: Word32 }    -- really 3 bits
  deriving (Eq,Ord,Read,Show,Num,Data,Typeable)

instance Bounded WireType where
  minBound = 0
  maxBound = 5

-- | 'FieldType' is the integer associated with the
-- FieldDescriptorProto's Type.  The allowed range is currently 1 to
-- 18.
newtype FieldType = FieldType { getFieldType :: Int } -- really [1..18] as fromEnum of Type from Type.hs
  deriving (Eq,Ord,Read,Show,Num,Data,Typeable)

instance Bounded FieldType where
  minBound = 1
  maxBound = 18

-- | 'EnumCode' is the Int32 assoicated with a
-- EnumValueDescriptorProto and is in the range 0 to 2^31-1
newtype EnumCode = EnumCode { getEnumCode :: Int32 }  -- really [0..maxBound::Int32] of some .proto defined enumeration
  deriving (Eq,Ord,Read,Show,Num,Data,Typeable) 

instance Bounded EnumCode where
  minBound = 0
  maxBound = 2147483647 -- 2^-31 -1 

-- | 'WireSize' is the Int64 size type associate with the lazy
-- bytestrings used in the 'Put' and 'Get' monads.
type WireSize = Int64

-- | The 'Mergeable' class is not a 'Monoid', 'mergeEmpty' is not a
-- left or right unit like 'mempty'.  The default 'mergeAppend' is to
-- take the second parameter and discard the first one.  The
-- 'mergeConcat' defaults to @foldl@ associativity.
class Mergeable a where
  -- | The 'mergeEmpty' value of a basic type or a message with
  -- required fields will be undefined and a runtime error to
  -- evaluate.  These are only handy for reading the wire encoding and
  -- users should employ 'defaultValue' instead'.
  mergeEmpty :: a
  mergeEmpty = error "You did not define Mergeable.mergeEmpty!"

  -- | 'mergeAppend' is the right-biased merge of two values.  A
  -- message (or group) is merged recursively.  Required field are
  -- always taken from the second message. Optional field values are
  -- taken from the most defined message or the message message if
  -- both are set.  Repeated fields have the sequences concatenated.
  -- Note that strings and bytes are NOT concatenated.
  mergeAppend :: a -> a -> a
  mergeAppend _a b = b

  -- | 'mergeConcat' is @ F.foldl mergeAppend mergeEmpty @ and this
  -- default definition is not overrring in any of the code.
  mergeConcat :: F.Foldable t => t a -> a
  mergeConcat = F.foldl mergeAppend mergeEmpty

-- | The Default class has the default-default values of types.  See
-- <http://code.google.com/apis/protocolbuffers/docs/proto.html#optional>
-- and also note that 'Enum' types have a 'defaultValue' that is the
-- first one in the @.proto@ file (there is always at least one
-- value).  Instances of this for messages hold any default value
-- defined in the @.proto@ file.
class Default a where
  -- | The 'defaultValue' is never undefined or an error to evalute.
  -- This makes it much more useful compared to 'mergeEmpty'. Optional
  -- field values are set to 'Just' and not 'Nothing'.  Repeated field
  -- value are always empty by default.
  defaultValue :: a

-- | If you mis-match a FieldType with the type of @b@ then you will
-- get a pattern match failure at runtime.
--
-- These are only used internally and by the generated files.  The
-- users should not (normally) need to import these class functions.
class Wire b where
  {-# INLINE wireSize #-}
  wireSize :: FieldType -> b -> WireSize
  {-# INLINE wirePut #-}
  wirePut :: FieldType -> b -> Put
  {-# INLINE wireGet #-}
  wireGet :: FieldType -> Get b
