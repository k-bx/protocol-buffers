module Text.ProtocolBuffers.WireMessage where

import Data.Map

{- The point of this module:

There needs to be a way to represent the bytes in a form that is
in-between the type specific messages and the single string of bytes.

Encoding and Deconding WireMessage does not respect ordering between
different FieldId but must respect ordering of repeated FieldId
values.

Converting WireData to and from the actual types (e.g. sint64) will be
done elsewhere.

Converting a WireData to a message type will require code generation
of a class instance.

-}

import Data.Bits(Bits(..))
import Data.Generics(Data(..),Typeable(..))
import Data.Word(Word32,Word64)
import Data.ByteString(ByteString)
import Data.Sequence(Seq)
import Data.Map(Map,unionWith)
import Data.Monoid(Monoid(..))

newtype FieldId = FieldId { getFieldID :: Word32 } -- really 29 bits, 0 to 2^29-1
  deriving (Eq,Ord,Show,Data,Typeable)

newtype WireId = WireId { getWireID :: Word32 } -- really 3 bits
  deriving (Eq,Ord,Show,Data,Typeable)

data WireData = Fix4 Word32 
              | Fix8 Word32 -- 4 and 8 byte fixed length types, lsb first on wire
              | VarInt ByteString -- the 128 bit variable encoding (least significant first)
              | VarString ByteString -- length of contents as a VarInt
                          ByteString -- the contents on the wire
  deriving (Eq,Ord,Show,Data,Typeable)

newtype WireMessage = WireMessage (Map FieldId (Seq WireData))

instance Monoid WireMessage where
  mempty = WireMessage mempty
  mappend (WireMessage a) (WireMessage b) = WireMessage (unionWith mappend a b)

wireId :: WireData -> WireId
wireId  (Fix4 {}) = WireId 5
wireId  (Fix8 {}) = WireId 1
wireId  (VarInt {}) = WireId 0
wireId  (VarString {}) = WireId 2

composeFieldWire :: FieldId -> WireId -> Word32
composeFieldWire (FieldId f) (WireId w) = (f `shiftL` 3) .|. w

decomposeFieldWire :: Word32 -> (FieldId,WireId)
decomposeFieldWire x = (FieldId (x `shiftR` 3), WireId (x .&. 7))

encodeWireMessage :: WireMessage -> ByteString
encodeWireMessage = undefined

decodeWireMessage :: ByteString -> WireMessage
decodeWireMessage = undefined

