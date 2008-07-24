-- http://code.google.com/apis/protocolbuffers/docs/encoding.html
module Text.ProtocolBuffers.WireMessage
    ( Wire(..),LazyResult(..),runGetOnLazy,runPut
    , size,lenSize,putSize
    , wireSizeReq,wireSizeOpt,wireSizeRep
    , wirePutReq,wirePutOpt,wirePutRep
    , getMessage,getBareMessage) where

{- The point of this module:

There needs to be a way to represent the bytes in a form that is
in-between the type specific messages and the single string of bytes.

Encoding and Deconding WireMessage does not respect ordering between
different FieldId but must respect ordering of repeated FieldId
values.

Converting a WireData to a message type will require code generation
of instance Wire.

-}
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.Mergeable

import Data.Bits (Bits(..))
import Data.Generics (Data(..),Typeable(..))
import Data.List (unfoldr,genericLength)
import Data.Map (Map,unionWith)
import Data.Monoid (Monoid(..))
import Data.Word (Word8)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as BS (length,pack,fromChunks)
import qualified Data.ByteString.Lazy.Internal as BS (ByteString(Empty,Chunk),chunk)
import qualified Data.Foldable as F(foldl',forM_)
import qualified Data.Sequence as Seq(length)
import qualified Data.Set as Set(notMember,delete,null)
-- GHC internals for getting at Double and Float representation as Word64 and Word32
import GHC.Exts (Double(D#),Float(F#),unsafeCoerce#)
import GHC.Word (Word64(W64#),Word32(W32#))

import Data.Binary.Put (Put,putWord8,putWord32be,putWord64be,putLazyByteString,runPut)
import Data.Binary.Builder (Builder)
import Data.Binary.Strict.Class (BinaryParser(getWord8,getWord32be,getWord64be,getByteString,bytesRead,isEmpty))
import qualified Data.Binary.Strict.IncrementalGet as Get (Get,runGet,Result(..))

-- import qualified Data.ByteString.Lazy as BS (unpack)
-- import Numeric

-- Make IncrementalGet run on the Lazy ByteStrings
data LazyResult r = Failed String
                  | Finished ByteString r
                  | Partial (ByteString -> LazyResult r)

runGetOnLazy :: Get.Get r r -> ByteString -> LazyResult r
runGetOnLazy parser (BS.Chunk x rest) = resolve rest $ Get.runGet parser x
runGetOnLazy parser BS.Empty = resolve BS.Empty $ Get.runGet parser mempty

resolve :: ByteString -> Get.Result r -> LazyResult r
resolve _rest (Get.Failed s)              = Failed s
resolve rest (Get.Finished b s)           = Finished (BS.chunk b rest) s
resolve (BS.Chunk x rest) (Get.Partial f) = resolve rest (f x)
resolve BS.Empty (Get.Partial f)          = newPartial
  where newPartial= Partial f'
        f' BS.Empty = newPartial
        f' (BS.Chunk x rest) = resolve rest (f x)


data WireSize = WireSize { childSize, internalSize :: !Int64 }

-- | 'size' takes the length of a primitive which is the same
-- internally and as a child of a message
size :: Int64 -> WireSize
size n = WireSize {childSize = n, internalSize = n}
-- | 'lenSize' takes the length of a bare message and adds its encoded
-- size as a header to get the 'childSize'.
lenSize :: Int64 -> WireSize
lenSize n = WireSize {childSize = n+size'Varint n, internalSize = n}

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
  wireGet :: BinaryParser get => FieldType -> get b

{-# INLINE wirePutReq #-}
wirePutReq :: Wire b => WireTag -> FieldType -> b -> Put
wirePutReq wireTag fieldType b = putWord32be (getWireTag wireTag) >> wirePut fieldType b

{-# INLINE wirePutOpt #-}
wirePutOpt :: Wire b => WireTag -> FieldType -> Maybe b -> Put
wirePutOpt wireTag fieldType Nothing = return ()
wirePutOpt wireTag fieldType (Just b) = putWord32be (getWireTag wireTag) >> wirePut fieldType b 

{-# INLINE wirePutRep #-}
wirePutRep :: Wire b => WireTag -> FieldType -> Seq b -> Put
wirePutRep wireTag fieldType bs = F.forM_ bs (\b -> putWord32be (getWireTag wireTag) >> wirePut fieldType b)

wireSizeReq :: Wire b => FieldType -> b -> Int64
wireSizeReq i v = 4 + childSize (wireSize i v)

wireSizeOpt :: Wire b => FieldType -> Maybe b -> Int64
wireSizeOpt _ Nothing = 0
wireSizeOpt i (Just v) = 4 + childSize (wireSize i v)

wireSizeRep :: Wire b => FieldType -> Seq b -> Int64
wireSizeRep i s = 4*(fromIntegral (Seq.length s)) + F.foldl' (\n v -> n+childSize(wireSize i v)) 0 s

putSize :: WireSize -> Put
putSize (WireSize {internalSize = x}) = putVarUInt x

-- getMessage assumes the wireTag for the message, if it existed, has already been read.
-- getMessage assumes that it still needs to read the Varint encoded length of the message.
getMessage :: forall get message. (BinaryParser get, Mergeable message, ReflectDescriptor message)
           => (FieldId -> message -> get message)
           -> get message
getMessage updater = do
  messageLength <- getVarInt
  stop <- fmap (messageLength+) bytesRead
  let go message reqs | Set.null reqs = go' message
                      | otherwise = do
        done <- fmap (stop<=) bytesRead
        if done then notEnoughData
          else do
            wireTag <- fmap WireTag getWord32be -- get tag off wire
            let (fieldId,wireType) = splitWireTag wireTag
            if Set.notMember wireTag allowed then unknown fieldId wireTag
              else do message' <- updater fieldId message
                      let reqs' = Set.delete wireTag reqs
                      go message' reqs'
      go' message = do
        done <- fmap (stop<=) bytesRead
        if done then return message
          else do
            wireTag <- fmap WireTag getWord32be -- get tag off wire
            let (fieldId,wireType) = splitWireTag wireTag
            if Set.notMember wireTag allowed then unknown fieldId wireType
              else updater fieldId message >>= go'
  go initialMessage required
 where
  initialMessage = mergeEmpty
  (GetMessageInfo {requiredTags=required,allowedTags=allowed}) = getMessageInfo initialMessage
  splitWireTag :: WireTag -> (FieldId,WireType)
  splitWireTag (WireTag wireTag) = ( FieldId . fromIntegral $ wireTag `shiftR` 3
                                   , WireType . fromIntegral $ wireTag .&. 7 )
  unknown fieldId wireType = fail ("WireMessage.getMessage: Unknown wire tag read: "
                                   ++ show (fieldId,wireType) ++ " when processing "
                                   ++ (show . descName . reflectDescriptorInfo $ initialMessage))
  notEnoughData = fail ("WireMessage.getMessage: Required fields missing when processing "
                        ++ (show . descName . reflectDescriptorInfo $ initialMessage))

-- getBareMessage assumes the wireTag for the message, if it existed, has already been read.
-- getBareMessage assumes that it does needs to read the Varint encoded length of the message.
-- getBareMessage will consume the entire ByteString it is operating on.
getBareMessage :: forall get message. (BinaryParser get, Mergeable message, ReflectDescriptor message)
           => (FieldId -> message -> get message)
           -> get message
getBareMessage updater = go initialMessage required
 where
  go message reqs | Set.null reqs = go' message
                  | otherwise = do
    done <- isEmpty
    if done then notEnoughData
      else do
        wireTag <- fmap WireTag getWord32be -- get tag off wire
        let (fieldId,wireType) = splitWireTag wireTag
        if Set.notMember wireTag allowed then unknown fieldId wireTag
          else do message' <- updater fieldId message
                  let reqs' = Set.delete wireTag reqs
                  go message' reqs'
  go' message = do
    done <- isEmpty
    if done then return message
      else do
        wireTag <- fmap WireTag getWord32be -- get tag off wire
        let (fieldId,wireType) = splitWireTag wireTag
        if Set.notMember wireTag allowed then unknown fieldId wireType
          else updater fieldId message >>= go'
  initialMessage = mergeEmpty
  (GetMessageInfo {requiredTags=required,allowedTags=allowed}) = getMessageInfo initialMessage
  splitWireTag :: WireTag -> (FieldId,WireType)
  splitWireTag (WireTag wireTag) = ( FieldId . fromIntegral $ wireTag `shiftR` 3
                                   , WireType . fromIntegral $ wireTag .&. 7 )
  unknown fieldId wireType = fail ("WireMessage.getMessage: Unknown wire tag read: "
                                   ++ show (fieldId,wireType) ++ " when processing "
                                   ++ (show . descName . reflectDescriptorInfo $ initialMessage))
  notEnoughData = fail ("WireMessage.getMessage: Required fields missing when processing "
                        ++ (show . descName . reflectDescriptorInfo $ initialMessage))

unknownField :: (BinaryParser get) => FieldId -> get a
unknownField fieldId = fail ("Impossible? Message claims theris is unknown field id on wire: "++show fieldId)

instance Wire Double where
  wireSize {- TYPE_DOUBLE -} 1     _ = size $ 8
  wirePut {- TYPE_DOUBLE -} 1 (D# d) = putWord64be (W64# (unsafeCoerce# d))
  wireGet {- TYPE_DOUBLE -} 1        = fmap (\(W64# w) -> D# (unsafeCoerce# w)) getWord64be

instance Wire Float where
  wireSize {- TYPE_FLOAT -} 2      _ = size $ 4
  wirePut {- TYPE_FLOAT -} 2  (F# f) = putWord32be (W32# (unsafeCoerce# f))
  wireGet {- TYPE_FLOAT -} 2         = fmap (\(W32# w) -> F# (unsafeCoerce# w)) getWord32be

instance Wire Int64 where
  wireSize {- TYPE_INT64 -} 3      x = size $ size'Varint x
  wireSize {- TYPE_SINT64 -} 18    x = size $ size'Varint (zzEncode64 x)
  wireSize {- TYPE_SFIXED64 -} 16  _ = size $ 8
  wirePut {- TYPE_INT64 -} 3       x = putVarSInt x
  wirePut {- TYPE_SINT64 -} 18     x = putVarUInt (zzEncode64 x)
  wirePut {- TYPE_SFIXED64 -} 16   x = putWord64be (fromIntegral x)
  wireGet {- TYPE_INT64 -} 3         = getVarInt
  wireGet {- TYPE_SINT64 -} 18       = fmap zzDecode64 getVarInt
  wireGet {- TYPE_SFIXED64 -} 16     = fmap fromIntegral getWord64be

instance Wire Int32 where
  wireSize {- TYPE_INT32 -} 5      x = size $ size'Varint x
  wireSize {- TYPE_SINT32 -} 17    x = size $ size'Varint (zzEncode32 x)
  wireSize {- TYPE_SFIXED32 -} 15  _ = size $ 4
  wirePut {- TYPE_INT32 -} 5       x = putVarSInt x
  wirePut {- TYPE_SINT32 -} 17     x = putVarUInt (zzEncode32 x)
  wirePut {- TYPE_SFIXED32 -} 15   x = putWord32be (fromIntegral x)
  wireGet {- TYPE_INT32 -} 5         = getVarInt
  wireGet {- TYPE_SINT32 -} 17       = fmap zzDecode32 getVarInt
  wireGet {- TYPE_SFIXED32 -} 15     = fmap fromIntegral getWord32be

instance Wire Word64 where
  wireSize {- TYPE_UINT64 -} 4     x = size $ size'Varint x
  wireSize {- TYPE_FIXED64 -} 6    _ = size $ 8
  wirePut {- TYPE_UINT64 -} 4      x = putVarUInt x
  wirePut {- TYPE_FIXED64 -} 6     x = putWord64be x
  wireGet {- TYPE_UINT64 -} 4        = getVarInt
  wireGet {- TYPE_FIXED64 -} 6       = getWord64be

instance Wire Word32 where
  wireSize {- TYPE_UINT32 -} 13    x = size $ size'Varint x
  wireSize {- TYPE_FIXED32 -} 7    _ = size $ 4
  wirePut {- TYPE_UINT32 -} 13     x = putVarUInt x
  wirePut {- TYPE_FIXED32 -} 7     x = putWord32be x
  wireGet {- TYPE_UINT32 -} 13       = getVarInt
  wireGet {- TYPE_FIXED32 -} 7       = getWord32be

instance Wire Bool where
  wireSize {- TYPE_BOOL -} 8       _ = size $ 1
  wirePut {- TYPE_BOOL -} 8    False = putWord8 0
  wirePut {- TYPE_BOOL -} 8    True  = putWord8 1 -- google's wire_format_inl.h
  wireGet {- TYPE_BOOL -} 8          = do
    (x :: Word32) <- getVarInt -- google's wire_format_inl.h line 97
    case x of
      0 -> return False
      x | x < 128 -> return True
      _ -> fail ("TYPE_BOOL read failure : " ++ show x)

instance Wire ByteString where
  wireSize {- TYPE_STRING -} 9     x = lenSize $ BS.length x
  wireSize {- TYPE_BYTES -} 12     x = lenSize $ BS.length x
  wirePut {- TYPE_STRING -} 9      x = putVarUInt (BS.length x) >> putLazyByteString x
  wirePut {- TYPE_BYTES -} 12      x = putVarUInt (BS.length x) >> putLazyByteString x
  wireGet {- TYPE_STRING -} 9        = getVarInt >>= getByteString >>= return . toLazy --getLazyByteString 
  wireGet {- TYPE_BYTES -} 12        = getVarInt >>= getByteString >>= return . toLazy --getLazyByteString

-- Wrap a protocol-buffer Enum in fromEnum or toEnum and serialize the Int:
instance Wire Int where
  wireSize {- TYPE_ENUM -} 14      x = size $ size'Varint x
  wirePut {- TYPE_ENUM -} 14       x = putVarUInt x
  wireGet {- TYPE_ENUM -} 14         = getVarInt

toLazy :: Strict.ByteString -> ByteString
toLazy = BS.fromChunks . (:[])

-- TYPE_GROUP 10
-- TYPE_MESSAGE 11
-- -- 

-- This will have to examine the value of positive numbers to get the size
{-# INLINE size'Varint #-}
size'Varint :: (Bits a,Integral a) => a -> Int64
size'Varint b = case compare b 0 of
                  LT -> fromIntegral (divBy (bitSize b) 7)
                  EQ -> 1
                  GT -> genericLength . takeWhile (0<) . iterate (`shiftR` 7) $ b

{-# INLINE divBy #-}
divBy :: (Ord a, Integral a) => a -> a -> a
divBy a b = let (q,r) = quotRem (abs a) b
            in if r==0 then q else succ q

-- Taken from google's code, but I had to explcitly add fromIntegral in the right places:
zzEncode32 :: Int32 -> Word32
zzEncode32 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 31))
zzEncode64 :: Int64 -> Word64
zzEncode64 x = fromIntegral ((x `shiftL` 1) `xor` (x `shiftR` 63))
zzDecode32 :: Word32 -> Int32
zzDecode32 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))
zzDecode64 :: Word64 -> Int64
zzDecode64 w = (fromIntegral (w `shiftR` 1)) `xor` (negate (fromIntegral (w .&. 1)))

-- The above is tricky, so the testing roundtrips and versus examples is needed:
testZZ = and (concat testsZZ)
  where testsZZ = [ map (\v -> v ==zzEncode64 (zzDecode64 v)) values
                  , map (\v -> v ==zzEncode32 (zzDecode32 v)) values
                  , map (\v -> v ==zzDecode64 (zzEncode64 v)) values
                  , map (\v -> v ==zzDecode32 (zzEncode32 v)) values
                  , [ zzEncode32 minBound == maxBound
                    , zzEncode32 maxBound == pred maxBound
                    , zzEncode64 minBound == maxBound
                    , zzEncode64 maxBound == pred maxBound
                    , zzEncode64 0 == 0,    zzEncode32 0 == 0
                    , zzEncode64 (-1) == 1, zzEncode32 (-1) == 1
                    , zzEncode64 1 == 2,    zzEncode32 1 == 2
                    ] ]
        values :: (Bounded a,Integral a) => [a]
        values = [minBound,div minBound 2,-3,-2,-1,0,1,2,3,div maxBound 2, maxBound]

{-# INLINE getVarInt #-}
getVarInt :: (Integral a, Bits a, BinaryParser get) => get a
getVarInt = do -- optimize first read instead of calling (go 0 0)
  b <- getWord8
  if testBit b 7 then go 7 (fromIntegral (b .&. 0x7F))
    else return (fromIntegral b)
 where
  go n val = do
    b <- getWord8
    if testBit b 7 then go (n+7) (val .|. ((fromIntegral (b .&. 0x7F)) `shiftL` n))
      else return (val .|. ((fromIntegral b) `shiftL` n))

-- This can be used on any Integral type and is needed for signed types; unsigned can use putVarUInt below.
{-# INLINE putVarSInt #-}
putVarSInt :: (Integral a, Bits a) => a -> Put
putVarSInt b =
  case compare b 0 of
    LT -> let len = divBy (bitSize b) 7               -- (pred len)*7 < bitSize b <= len*7
              last'Size = (bitSize b)-((pred len)*7)  -- at least 1 and at most 7
              last'Mask = pred (1 `shiftL` last'Size) -- at least 1 and at most 255
              go i 1 = putWord8 (fromIntegral i .&. last'Mask)
              go i n = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> go (i `shiftR` 7) (pred n)
          in go b len
    EQ -> putWord8 0
    GT -> putVarUInt b

-- This should be used on unsigned Integral types only (not checked)
{-# INLINE putVarUInt #-}
putVarUInt :: (Integral a, Bits a) => a -> Put
putVarUInt b = let go i | i < 0x80 = putWord8 (fromIntegral i)
                        | otherwise = putWord8 (fromIntegral (i .&. 0x7F) .|. 0x80) >> go (i `shiftR` 7)
               in go b

{-
-- copied from Data.Binary.Builder
-- copied from Data.ByteString.Lazy
--
defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

-- | /O(n)./ Extract a lazy 'L.ByteString' from a 'Builder'.
-- The construction work takes place if and when the relevant part of
-- the lazy 'L.ByteString' is demanded.
--
toLazyByteStringSized :: Int64 -> Builder -> ByteString
toLazyByteStringSized m bytes = BS.fromChunks $ unsafePerformIO $ do
    buf <- newBuffer bytes
    return (runBuilder (m `append` flush) (const []) buf)

newBuffer :: Int -> IO Buffer
newBuffer size = do
    fp <- S.mallocByteString size
    return $! Buffer fp 0 0 size
{-# INLINE newBuffer #-}

runSizedPut :: Int64 -> Put -> ByteString
runSizedPut bytes | bytes<0 = error "runSizedPut : size cannot be negative"
                  | bytes==0 = const mempty
                  | defaultSize<=bytes = runPut
                  | otherwise = toLazyByteStringSized bytes . sndS  . unPut $ put
-}

{-

{- Useful for testing -}

testVarInt :: (Integral a, Enum a, Ord a, Bits a) => a -> (Bool,[Word8],Either String a)
testVarInt i = let w = toVarInt i
               in case fromVarInt w of
                    r@(Right v) -> (v==i,w,r)
                    l -> (False,w,l)

fromVarInt :: (Integral a, Bits a) => [Word8] -> Either String a
fromVarInt [] = Left "No bytes!"
fromVarInt (b:bs) = do
  if testBit b 7 then go bs 7 (fromIntegral (b .&. 0x7F))
    else if null bs then Right (fromIntegral b)
           else Left ("Excess bytes: " ++ show (b,bs))
 where
  go [] n val = Left ("Not enough bytes: " ++ show (n,val))
  go (b:bs) n val = do
    if testBit b 7 then go bs (n+7) (val .|. ((fromIntegral (b .&. 0x7F)) `shiftL` n))
      else if null bs then Right (val .|. ((fromIntegral b) `shiftL` n))
             else Left ("Excess bytes: " ++ show (b,bs,n,val))

toVarInt :: (Integral a, Bits a) => a -> [Word8]
toVarInt b = case compare b 0 of
               LT -> let len = divBy (bitSize b) 7
                         last'Size = (bitSize b) - ((pred len)*7)
                         last'Mask = pred (1 `shiftL` last'Size)
                         go i 1 = [fromIntegral i .&. last'Mask]
                         go i n = (fromIntegral (i .&. 0x7F) .|. 0x80) : go (i `shiftR` 7) (pred n)
                     in go b len
               EQ -> [0]
               GT -> let go i | i < 0x80 = [fromIntegral i]
                              | otherwise = (fromIntegral (i .&. 0x7F) .|. 0x80) : go (i `shiftR` 7)
                     in go b
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
                 b = Build.toLazyByteString (Build.putWord64be w)
             in map (flip showHex "") $  BS.unpack  b

fle :: Double -> [String]
fle (D# d) = let w = W64# (unsafeCoerce# d)
                 b = Build.toLazyByteString (Build.putWord64le w)
             in map (flip showHex "") $ BS.unpack  b

gbe :: [Char] -> ([Char], ([Char], Word64, String), ([Char], Double))
gbe s = let pairs = Data.List.unfoldr (\a -> if Prelude.null a then Nothing
                                               else Just (splitAt 2 a)) s
            words = map (fst . head . readHex) pairs
            w@(W64# w64) = Get.runGet Get.getWord64be (BS.pack words)
            d = D# (unsafeCoerce# w64)
        in (s,("word",w,showHex w ""),("double",d))

gle :: [Char] -> ([Char], ([Char], Word64, String), ([Char], Double))
gle s = let pairs = Data.List.unfoldr (\a -> if Prelude.null a then Nothing
                                               else Just (splitAt 2 a)) s
            words = map (fst . head . readHex) pairs
            w@(W64# w64) = Get.runGet Get.getWord64le (BS.pack words)
            d = D# (unsafeCoerce# w64)
        in (s,("word",w,showHex w ""),("double",d))

-}


-- Some to-be-reviewed-for-sanity prototypes for the bytestream reader

data WireData = VarInt ByteString -- the 128 bit variable encoding (least significant first)
              | Fix8 ByteString -- 4 and 8 byte fixed length types, lsb first on wire
              | VarString ByteString -- length of contents as a VarInt
              |           ByteString -- the contents on the wire
              | StartGroup
              | StopGroup
              | Fix4 ByteString
  deriving (Eq,Ord,Show,Data,Typeable)

newtype WireMessage = WireMessage (Map FieldId (Seq WireData))

instance Monoid WireMessage where
  mempty = WireMessage mempty
  mappend (WireMessage a) (WireMessage b) = WireMessage (unionWith mappend a b)

wireId :: WireData -> WireType
wireId (VarInt {})    = WireType 0
wireId (Fix8 {})      = WireType 1
wireId (VarString {}) = WireType 2
wireId  StartGroup    = WireType 3
wireId  StopGroup     = WireType 4
wireId (Fix4 {})      = WireType 5

checkWireType :: FieldType -> WireType -> Bool
checkWireType  1 1 = True -- DOUBLE
checkWireType  2 5 = True -- FLOAT
checkWireType  3 0 = True -- INT64
checkWireType  4 0 = True -- UNT64
checkWireType  5 0 = True -- INT32
checkWireType  6 1 = True -- FIXED64
checkWireType  7 5 = True -- FIXED32
checkWireType  8 0 = True -- BOOL
checkWireType  9 2 = True -- STRING
checkWireType 10 3 = True -- StartGroup
checkWireType 10 4 = True -- EndGroup
checkWireType 11 2 = True -- MESSAGE
checkWireType 12 2 = True -- BYTES
checkWireType 13 0 = True -- UINT32
checkWireType 14 0 = True -- ENUM
checkWireType 15 5 = True -- SFIXED32
checkWireType 16 1 = True -- SFIXED64
checkWireType 17 5 = True -- SINT32
checkWireType 18 1 = True -- SINT64
checkWireType  x y = False -- fail ("WireMessage.checkWireType: Mismatch of type Id "++show x ++ " and wire type " ++ show y ++ ".")

composeFieldWire :: FieldId -> WireType -> Word32
composeFieldWire (FieldId f) (WireType w) = ((fromIntegral f) `shiftL` 3) .|. w

decomposeFieldWire :: Word32 -> (FieldId,WireType)
decomposeFieldWire x = (FieldId (fromIntegral (x `shiftR` 3)), WireType (x .&. 7))

encodeWireMessage :: WireMessage -> ByteString
encodeWireMessage = undefined

decodeWireMessage :: ByteString -> WireMessage
decodeWireMessage = undefined

