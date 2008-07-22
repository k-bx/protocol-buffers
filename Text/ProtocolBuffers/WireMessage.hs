module Text.ProtocolBuffers.WireMessage where

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
import Text.ProtocolBuffers.Basic

import Data.Bits(Bits(..))
import qualified Data.ByteString.Lazy as BS (length,pack)
import Data.Generics(Data(..),Typeable(..))
import Data.List(unfoldr)
import Data.Map(Map,unionWith)
import Data.Monoid(Monoid(..))
import Data.Word(Word8)

import Data.Binary.Put as Put
import Data.Binary.Get as Get
import Data.Binary.Builder as Build

import GHC.Exts
import GHC.Word
import Numeric

{-  On my G4 (big endian) powerbook:

le is the protocol-buffer standard (x86 optimized)

*Text.ProtocolBuffers.WireMessage Data.Int Data.Word Numeric> gle . cw $ fle pi
("182d4454fb210940",("word",4614256656552045848,"400921fb54442d18"),("double",3.141592653589793))

be is the network byte order standard (and native for my G4)

*Text.ProtocolBuffers.WireMessage Data.Int Data.Word Numeric> gbe . cw $ fbe pi
("400921fb54442d18",("word",4614256656552045848,"400921fb54442d18"),("double",3.141592653589793))

-}
padL n c s = let l = length s
             in replicate (n-l) c ++ s

cw = concatMap (padL 2 '0')

fbe :: Double -> [String]
fbe (D# d) = let w = W64# (unsafeCoerce# d)
                 b = toLazyByteString (Build.putWord64be w)
             in map (flip showHex "") $  BS.unpack  b

fle :: Double -> [String]
fle (D# d) = let w = W64# (unsafeCoerce# d)
                 b = toLazyByteString (Build.putWord64le w)
             in map (flip showHex "") $ BS.unpack  b

gbe s = let pairs = Data.List.unfoldr (\a -> if Prelude.null a then Nothing
                                               else Just (splitAt 2 a)) s
            words = map (fst . head . readHex) pairs
            w@(W64# w64) = Get.runGet Get.getWord64be (BS.pack words)
            d = D# (unsafeCoerce# w64)
        in (s,("word",w,showHex w ""),("double",d))

gle s = let pairs = Data.List.unfoldr (\a -> if Prelude.null a then Nothing
                                               else Just (splitAt 2 a)) s
            words = map (fst . head . readHex) pairs
            w@(W64# w64) = Get.runGet Get.getWord64le (BS.pack words)
            d = D# (unsafeCoerce# w64)
        in (s,("word",w,showHex w ""),("double",d))

class StorableVar a where
  itemSize :: a -> Int -- can examine the item, unlike Storable

-- http://code.google.com/apis/protocolbuffers/docs/encoding.html

---- ---- Compute sizes ---- ----

-- This should work for all fixed-width Ints and Words
{-# INLINE size'Varint #-}
size'Varint b = case compare b 0 of
                  LT -> divBy (bitSize b) 7
                  EQ -> 1
                  GT -> length . takeWhile (0<) . iterate (`shiftR` 7) $ b

divBy a b = let (q,r) = quotRem (abs a) b
            in if r==0 then q else succ q

size'DOUBLE :: Double -> Int
size'DOUBLE = const 8
size'FLOAT :: Float -> Int
size'FLOAT = const 4
size'INT64 :: Int64 -> Int
size'INT64 = size'Varint
size'UINT64 :: Int64 -> Int
size'UINT64 = size'Varint
size'INT32 :: Int32 -> Int
size'INT32 = size'Varint
size'FIXED64 :: Word64 -> Int
size'FIXED64 = const 8
size'FIXED32 :: Word32 -> Int
size'FIXED32 = const 4
size'BOOL :: Bool -> Int
size'BOOL = const 1
size'STRING :: ByteString -> Int
size'STRING bs = size'Varint len + len where len = BS.length bs
--size'GROUP
--size'MESSAGE
size'BYTES :: ByteString -> Int
size'BYTES bs = size'Varint len + len where len = BS.length bs
size'UINT32 :: Word32 -> Int
size'UINT32 = size'Varint
{-# INLINE size'ENUM #-}
size'ENUM :: (Enum e) => e -> Int
size'ENUM = size'Varint . fromEnum
size'SFIXED32 :: Int32 -> Int
size'SFIXED32 = const 4
size'SFIXED64 :: Int64 -> Int
size'SFIXED64 = const 8
size'SINT32 :: Int32 -> Int
size'SINT32 x = size'Varint ((x `shiftL` 1) `xor` (x `shiftR` 31))
size'SINT64 :: Int64 -> Int
size'SINT64 x = size'Varint ((x `shiftL` 1) `xor` (x `shiftR` 63))

-- -- 

toVarInt :: (Enum a, Ord a, Bits a) => a -> [Word8]
toVarInt b = case compare b 0 of
               LT -> let len = divBy (bitSize b) 7
                         last'Size = (len*7)-(bitSize b)
                         last'Mask = pred (1 `shiftL` last'Size)
                         go i 1 = [to8 i .&. last'Mask]
                         go i n = (to8 (i .&. 0x7F) .|. 0x80) : go (i `shiftR` 7) (pred n)
                     in go b len
               EQ -> [0]
               GT -> let go i | i < 0x80 = [to8 i]
                              | otherwise = (to8 (i .&. 0x7F) .|. 0x80) : go (i `shiftR` 7)
                     in go b
  where {-# INLINE to8 #-}
        to8 = toEnum . fromEnum

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

