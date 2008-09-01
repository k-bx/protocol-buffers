-- http://code.google.com/apis/protocolbuffers/docs/encoding.html
{- | This module cooperates with the generated code to implement the
  Wire instances.  

  enum WireType {
    WIRETYPE_VARINT           = 0,
    WIRETYPE_FIXED64          = 1,
    WIRETYPE_LENGTH_DELIMITED = 2,
    WIRETYPE_START_GROUP      = 3,
    WIRETYPE_END_GROUP        = 4,
    WIRETYPE_FIXED32          = 5,
 -}
module Text.ProtocolBuffers.WireMessage
    ( messageSize,messagePut,messageGet,messagePutM,messageGetM
    , runGet,runGetOnLazy,runPut,size'Varint,toWireType,toWireTag,mkWireTag
    , Wire(..),WireSize,Put,Get
    , putSize,putVarUInt,getVarInt,putLazyByteString,splitWireTag
    , wireSizeReq,wireSizeOpt,wireSizeRep
    , wirePutReq,wirePutOpt,wirePutRep
    , getMessage,getBareMessage,getMessageWith
    , unknownField
    , castWord64ToDouble,castWord32ToFloat,castDoubleToWord64,castFloatToWord32
    , zzEncode64,zzEncode32,zzDecode64,zzDecode32
    ) where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Reflections(ReflectDescriptor(reflectDescriptorInfo,getMessageInfo)
                                       ,DescriptorInfo(..),GetMessageInfo(..))
import Text.ProtocolBuffers.Mergeable(Mergeable(mergeEmpty))

import Data.Bits (Bits(..))
import Data.Typeable (Typeable(..))
import Data.List (genericLength)
import qualified Data.ByteString as Strict (ByteString)
import qualified Data.ByteString.Lazy as BS (length,pack,fromChunks,toChunks,drop)
import qualified Data.ByteString.Lazy.Internal as BS (ByteString(Empty,Chunk),chunk)
import qualified Data.Foldable as F(foldl',forM_)
import qualified Data.Sequence as Seq(length)
import qualified Data.Set as Set(notMember,delete,null)
-- GHC internals for getting at Double and Float representation as Word64 and Word32
import GHC.Exts (Double(D#),Float(F#),unsafeCoerce#)
import GHC.Word (Word64(W64#),Word32(W32#))

import Data.Binary.Put (Put,runPut,putWord8,putWord32be,putWord64be,putLazyByteString)
--import Data.Binary.Builder (Builder)
import Text.ProtocolBuffers.Get as Get (Result(..),Get,runGet,lookAhead,spanOf,skip,bytesRead,isEmpty
                                       ,getWord8,getWord32be,getWord64be,getLazyByteString,getByteString)

-- import qualified Data.ByteString.Lazy as BS (unpack)
-- import Numeric

runGetOnLazy :: Get r -> ByteString -> Either String (r,ByteString)
runGetOnLazy parser bs = resolve (Get.runGet parser bs)

resolve :: Get.Result r -> Either String (r,ByteString)
resolve (Get.Failed i s) = Left ("Failed at "++show i++" : "++s)
resolve (Get.Finished bs _i r) = Right (r,bs)
resolve (Get.Partial {}) = Left ("Not enough input")

-- Make IncrementalGet run on the Lazy ByteStrings
{-
data LazyResult r = Failed String
                  | Finished ByteString r
                  | Partial (ByteString -> LazyResult r)

runGetOnLazy :: Get r r -> ByteString -> Either String (r,ByteString)
runGetOnLazy parser (BS.Chunk x rest) = resolve rest $ Get.runGet parser x
runGetOnLazy parser BS.Empty = resolve BS.Empty $ Get.runGet parser mempty

resolve :: ByteString -> Get.Result r -> Either String (r,ByteString)
resolve _rest (Get.Failed s)              = Left s
resolve rest (Get.Finished b s)           = Right (s,(BS.chunk b rest))
resolve (BS.Chunk x rest) (Get.Partial f) = resolve rest (f x)
resolve BS.Empty (Get.Partial f)          = Left "Insufficient input"
-}
prependMessageSize :: WireSize -> WireSize
prependMessageSize n = n + size'Varint n

messageSize :: (ReflectDescriptor msg,Wire msg) => msg -> WireSize
messageSize msg = prependMessageSize (wireSize 11 msg)

messagePut :: (ReflectDescriptor msg, Wire msg) => msg -> ByteString
messagePut msg = runPut (wirePut 11 msg)

messagePutM :: (ReflectDescriptor msg, Wire msg) => msg -> Put
messagePutM msg = wirePut 11 msg

messageGet :: (ReflectDescriptor msg, Wire msg) => ByteString -> Either String (msg,ByteString)
messageGet bs = runGetOnLazy (wireGet 11) bs

messageGetM :: (ReflectDescriptor msg, Wire msg) => Get msg
messageGetM = wireGet 11

{-# INLINE wirePutReq #-}
wirePutReq :: Wire b => WireTag -> FieldType -> b -> Put
-- -- -- wirePutReq wireTag 11 b = putVarUInt (getWireTag wireTag) >> putVarUInt (wireSize 11 b) >> wirePut 11 b
wirePutReq wireTag 10 b = let startTag = getWireTag wireTag
                              endTag = succ startTag
                          in putVarUInt startTag >> wirePut 10 b >> putVarUInt endTag
wirePutReq wireTag fieldType b = putVarUInt (getWireTag wireTag) >> wirePut fieldType b

{-# INLINE wirePutOpt #-}
wirePutOpt :: Wire b => WireTag -> FieldType -> Maybe b -> Put
wirePutOpt _wireTag _fieldType Nothing = return ()
wirePutOpt wireTag fieldType (Just b) = wirePutReq wireTag fieldType b 

{-# INLINE wirePutRep #-}
wirePutRep :: Wire b => WireTag -> FieldType -> Seq b -> Put
wirePutRep wireTag fieldType bs = F.forM_ bs (\b -> wirePutReq wireTag fieldType b)

{-# INLINE wireSizeReq #-}
wireSizeReq :: Wire b => Int64 -> FieldType -> b -> Int64
-- -- -- wireSizeReq tagSize 11 v = tagSize + prependMessageSize (wireSize 11 v)
wireSizeReq tagSize 10 v = tagSize + wireSize 10 v + tagSize
wireSizeReq tagSize 11 v = tagSize + prependMessageSize (wireSize 11 v)
wireSizeReq tagSize  i v = tagSize + wireSize i v

{-# INLINE wireSizeOpt #-}
wireSizeOpt :: Wire b => Int64 -> FieldType -> Maybe b -> Int64
wireSizeOpt _tagSize _i Nothing = 0
wireSizeOpt tagSize i (Just v) = wireSizeReq tagSize i v

{-# INLINE wireSizeRep #-}
wireSizeRep :: Wire b => Int64 -> FieldType -> Seq b -> Int64
wireSizeRep tagSize i s = F.foldl' (\n v -> n + wireSizeReq tagSize i v) 0 s

putSize :: WireSize -> Put
putSize = putVarUInt

toWireTag :: FieldId -> FieldType -> WireTag
toWireTag fieldId fieldType
    = ((fromIntegral . getFieldId $ fieldId) `shiftL` 3) .|. (fromIntegral . getWireType . toWireType $ fieldType)

mkWireTag :: FieldId -> WireType -> WireTag
mkWireTag fieldId fieldType
    = ((fromIntegral . getFieldId $ fieldId) `shiftL` 3) .|. (fromIntegral . getWireType $ fieldType)

splitWireTag :: WireTag -> (FieldId,WireType)
splitWireTag (WireTag wireTag) = ( FieldId . fromIntegral $ wireTag `shiftR` 3
                                 , WireType . fromIntegral $ wireTag .&. 7 )

getMessage :: (Mergeable message, ReflectDescriptor message,Typeable message)
           => (FieldId -> message -> Get message)           -- handles "allowed" wireTags
           -> Get message
getMessage = getMessageWith unknown

-- getMessage assumes the wireTag for the message, if it existed, has already been read.
-- getMessage assumes that it still needs to read the Varint encoded length of the message.
getMessageWith :: forall message. (Mergeable message, ReflectDescriptor message)
               => (FieldId -> WireType -> message -> Get message) -- handle wireTags that updater cannot
               -> (FieldId -> message -> Get message)           -- handles "allowed" wireTags
               -> Get message
getMessageWith punt updater = do
  messageLength <- getVarInt
  start <- bytesRead
  let stop = messageLength+start
      -- switch from go to go' once all the required fields have been found
      go reqs message | Set.null reqs = go' message
                      | otherwise = do
        here <- bytesRead
        case compare stop here of
          EQ -> notEnoughData messageLength start
          LT -> tooMuchData messageLength start here
          GT -> do
            wireTag <- fmap WireTag getVarInt -- get tag off wire
            let (fieldId,wireType) = splitWireTag wireTag
            if Set.notMember wireTag allowed then punt fieldId wireType message
              else let reqs' = Set.delete wireTag reqs
                   in updater fieldId message >>= go reqs'
      go' message = do
        here <- bytesRead
        case compare stop here of
          EQ -> return message
          LT -> tooMuchData messageLength start here
          GT -> do
            wireTag <- fmap WireTag getVarInt -- get tag off wire
            let (fieldId,wireType) = splitWireTag wireTag
            if Set.notMember wireTag allowed then punt fieldId wireType message
              else updater fieldId message >>= go'
  go required initialMessage
 where
  initialMessage = mergeEmpty
  (GetMessageInfo {requiredTags=required,allowedTags=allowed}) = getMessageInfo initialMessage
  notEnoughData messageLength start =
      fail ("Text.ProtocolBuffers.WireMessage.getMessage: Required fields missing when processing "
            ++ (show . descName . reflectDescriptorInfo $ initialMessage)
            ++ " at (messageLength,start) == " ++ show (messageLength,start))
  tooMuchData messageLength start here =
      fail ("Text.ProtocolBuffers.WireMessage.getMessage : overran expected length when processing"
            ++ (show . descName . reflectDescriptorInfo $ initialMessage)
            ++ " at  (messageLength,start,here) == " ++ show (messageLength,start,here))

unknown :: (Typeable a,ReflectDescriptor a) => FieldId -> WireType -> a -> Get a
unknown fieldId wireType initialMessage = do
  here <- bytesRead
  fail ("Text.ProtocolBuffers.WireMessage.getMessage: Unknown wire tag read (type,fieldId,wireType,here) == "
        ++ show (typeOf initialMessage,fieldId,wireType,here) ++ " when processing "
        ++ (show . descName . reflectDescriptorInfo $ initialMessage))


-- getBareMessage assumes the wireTag for the message, if it existed, has already been read.
-- getBareMessage assumes that it does needs to read the Varint encoded length of the message.
-- getBareMessage will consume the entire ByteString it is operating on, or until it
-- finds any STOP_GROUP tag
getBareMessage :: forall message. (Mergeable message, ReflectDescriptor message)
           => (FieldId -> message -> Get message)
           -> Get message
getBareMessage updater = go required initialMessage
 where
  go reqs message | Set.null reqs = go' message
                  | otherwise = do
    done <- isEmpty
    if done then notEnoughData
      else do
        wireTag <- fmap WireTag getWord32be -- get tag off wire
        let (fieldId,wireType) = splitWireTag wireTag
        if wireType == 4 then notEnoughData -- END_GROUP too soon
          else if Set.notMember wireTag allowed then go'unknown fieldId wireTag
                 else let reqs' = Set.delete wireTag reqs
                      in updater fieldId message >>= go reqs'
  go' message = do
    done <- isEmpty
    if done then return message
      else do
        wireTag <- fmap WireTag getWord32be -- get tag off wire
        let (fieldId,wireType) = splitWireTag wireTag -- WIRETYPE_END_GROUP
        if wireType == 4 then return message
          else if Set.notMember wireTag allowed then go'unknown fieldId wireType
                 else updater fieldId message >>= go'
  initialMessage = mergeEmpty
  (GetMessageInfo {requiredTags=required,allowedTags=allowed}) = getMessageInfo initialMessage
  go'unknown fieldId wireType = fail ("Text.ProtocolBuffers.WireMessage.getBareMessage: Unknown wire tag read: "
                                   ++ show (fieldId,wireType) ++ " when processing "
                                   ++ (show . descName . reflectDescriptorInfo $ initialMessage))
  notEnoughData = fail ("Text.ProtocolBuffers.WireMessage.getBareMessage: Required fields missing when processing "
                        ++ (show . descName . reflectDescriptorInfo $ initialMessage))

unknownField :: FieldId -> Get a
unknownField fieldId = do 
  here <- bytesRead
  fail ("Impossible? Text.ProtocolBuffers.WireMessage.unknownField "
        ++" The Message's updater claims there is an unknown field id on wire: "++show fieldId
        ++" at a position just before here == "++show here)

{-# INLINE castWord64ToDouble #-}
castWord64ToDouble :: Word64 -> Double
castWord64ToDouble (W64# w) = D# (unsafeCoerce# w)
{-# INLINE castWord32ToFloat #-}
castWord32ToFloat :: Word32 -> Float
castWord32ToFloat (W32# w) = F# (unsafeCoerce# w)
{-# INLINE castDoubleToWord64 #-}
castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 (D# d) = W64# (unsafeCoerce# d)
{-# INLINE castFloatToWord32 #-}
castFloatToWord32 :: Float -> Word32
castFloatToWord32 (F# d) = W32# (unsafeCoerce# d)

wireSizeErr :: Typeable a => FieldType -> a -> WireSize
wireSizeErr ft x = error $ concat [ "Impossible? wireSize field type mismatch error: Field type number ", show ft
                                  , " does not match internal type ", show (typeOf x) ]
wirePutErr :: Typeable a => FieldType -> a -> Put
wirePutErr ft x = fail $ concat [ "Impossible? wirePut field type mismatch error: Field type number ", show ft
                                , " does not match internal type ", show (typeOf x) ]
wireGetErr :: forall a. (Typeable a) => FieldType -> Get a
wireGetErr ft = fail $ concat [ "Impossible? wireGet field type mismatch error: Field type number ", show ft
                              , " does not match internal type ", show (typeOf (undefined :: a)) ]

instance Wire Double where
  wireSize {- TYPE_DOUBLE   -} 1      _ = 8
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_DOUBLE   -} 1      x = putWord64be (castDoubleToWord64 x)
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_DOUBLE   -} 1        = fmap castWord64ToDouble getWord64be
  wireGet ft = wireGetErr ft

instance Wire Float where
  wireSize {- TYPE_FLOAT    -} 2      _ = 4
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_FLOAT    -} 2      x = putWord32be (castFloatToWord32 x)
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_FLOAT    -} 2        = fmap castWord32ToFloat getWord32be
  wireGet ft = wireGetErr ft

instance Wire Int64 where
  wireSize {- TYPE_INT64    -} 3      x = size'Varint x
  wireSize {- TYPE_SINT64   -} 18     x = size'Varint (zzEncode64 x)
  wireSize {- TYPE_SFIXED64 -} 16     _ = 8
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_INT64    -} 3      x = putVarSInt x
  wirePut  {- TYPE_SINT64   -} 18     x = putVarUInt (zzEncode64 x)
  wirePut  {- TYPE_SFIXED64 -} 16     x = putWord64be (fromIntegral x)
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_INT64    -} 3        = getVarInt
  wireGet  {- TYPE_SINT64   -} 18       = fmap zzDecode64 getVarInt
  wireGet  {- TYPE_SFIXED64 -} 16       = fmap fromIntegral getWord64be
  wireGet ft = wireGetErr ft

instance Wire Int32 where
  wireSize {- TYPE_INT32    -} 5      x = size'Varint x
  wireSize {- TYPE_SINT32   -} 17     x = size'Varint (zzEncode32 x)
  wireSize {- TYPE_SFIXED32 -} 15     _ = 4
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_INT32    -} 5      x = putVarSInt x
  wirePut  {- TYPE_SINT32   -} 17     x = putVarUInt (zzEncode32 x)
  wirePut  {- TYPE_SFIXED32 -} 15     x = putWord32be (fromIntegral x)
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_INT32    -} 5        = getVarInt
  wireGet  {- TYPE_SINT32   -} 17       = fmap zzDecode32 getVarInt
  wireGet  {- TYPE_SFIXED32 -} 15       = fmap fromIntegral getWord32be
  wireGet ft = wireGetErr ft

instance Wire Word64 where
  wireSize {- TYPE_UINT64   -} 4      x = size'Varint x
  wireSize {- TYPE_FIXED64  -} 6      _ = 8
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_UINT64   -} 4      x = putVarUInt x
  wirePut  {- TYPE_FIXED64  -} 6      x = putWord64be x
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_FIXED64  -} 6        = getWord64be
  wireGet  {- TYPE_UINT64   -} 4        = getVarInt
  wireGet ft = wireGetErr ft

instance Wire Word32 where
  wireSize {- TYPE_UINT32   -} 13     x = size'Varint x
  wireSize {- TYPE_FIXED32  -} 7      _ = 4
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_UINT32   -} 13     x = putVarUInt x
  wirePut  {- TYPE_FIXED32  -} 7      x = putWord32be x
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_UINT32   -} 13       = getVarInt
  wireGet  {- TYPE_FIXED32  -} 7        = getWord32be
  wireGet ft = wireGetErr ft

instance Wire Bool where
  wireSize {- TYPE_BOOL     -} 8      _ = 1
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_BOOL     -} 8  False = putWord8 0
  wirePut  {- TYPE_BOOL     -} 8  True  = putWord8 1 -- google's wire_format_inl.h
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_BOOL     -} 8        = do
    x <- getVarInt :: Get Int32 -- google's wire_format_inl.h line 97
    case x of
      0 -> return False
      x' | x' < 128 -> return True
      _ -> fail ("TYPE_BOOL read failure : " ++ show x)
  wireGet ft = wireGetErr ft

instance Wire Utf8 where
-- items of TYPE_STRING is already in a UTF8 encoded Data.ByteString.Lazy
  wireSize {- TYPE_STRING   -} 9      x = prependMessageSize $ BS.length (utf8 x)
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_STRING   -} 9      x = putVarUInt (BS.length (utf8 x)) >> putLazyByteString (utf8 x)
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_STRING   -} 9        = getVarInt >>= getLazyByteString >>= return . Utf8
  wireGet ft = wireGetErr ft

instance Wire ByteString where
-- items of TYPE_BYTES is an untyped binary Data.ByteString.Lazy
  wireSize {- TYPE_BYTES    -} 12     x = prependMessageSize $ BS.length x
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_BYTES    -} 12     x = putVarUInt (BS.length x) >> putLazyByteString x
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_BYTES    -} 12       = getVarInt >>= getLazyByteString >>= return
  wireGet ft = wireGetErr ft

-- Wrap a protocol-buffer Enum in fromEnum or toEnum and serialize the Int:
instance Wire Int where
  wireSize {- TYPE_ENUM    -} 14      x = size'Varint x
  wireSize ft x = wireSizeErr ft x
  wirePut  {- TYPE_ENUM    -} 14      x = putVarUInt x
  wirePut ft x = wirePutErr ft x
  wireGet  {- TYPE_ENUM    -} 14        = getVarInt
  wireGet ft = wireGetErr ft

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

{-
-- The above is tricky, so the testing roundtrips and versus examples is needed:
testZZ :: Bool
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
-}
{-# INLINE getVarInt #-}
getVarInt :: (Integral a, Bits a) => Get a
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
  enum WireType {
    WIRETYPE_VARINT           = 0,
    WIRETYPE_FIXED64          = 1,
    WIRETYPE_LENGTH_DELIMITED = 2,
    WIRETYPE_START_GROUP      = 3,
    WIRETYPE_END_GROUP        = 4,
    WIRETYPE_FIXED32          = 5, };

    TYPE_DOUBLE         = 1;
    TYPE_FLOAT          = 2;
    TYPE_INT64          = 3;
    TYPE_UINT64         = 4;
    TYPE_INT32          = 5;
    TYPE_FIXED64        = 6;
    TYPE_FIXED32        = 7;
    TYPE_BOOL           = 8;
    TYPE_STRING         = 9;
    TYPE_GROUP          = 10;  // Tag-delimited aggregate.
    TYPE_MESSAGE        = 11;
    TYPE_BYTES          = 12;
    TYPE_UINT32         = 13;
    TYPE_ENUM           = 14;
    TYPE_SFIXED32       = 15;
    TYPE_SFIXED64       = 16;
    TYPE_SINT32         = 17;
    TYPE_SINT64         = 18; -}
-- http://code.google.com/apis/protocolbuffers/docs/encoding.html
toWireType :: FieldType -> WireType
toWireType  1 =  1
toWireType  2 =  5
toWireType  3 =  0
toWireType  4 =  0
toWireType  5 =  0
toWireType  6 =  1
toWireType  7 =  5
toWireType  8 =  0
toWireType  9 =  2
toWireType 10 =  3 -- START_GROUP
toWireType 11 =  2
toWireType 12 =  2
toWireType 13 =  0
toWireType 14 =  0
toWireType 15 =  5
toWireType 16 =  1
toWireType 17 =  5
toWireType 18 =  1
toWireType  x = error $ "Text.ProcolBuffers.Basic.toWireType: Bad FieldType: "++show x

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


{-
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

composeFieldWire :: FieldId -> WireType -> Word32
composeFieldWire (FieldId f) (WireType w) = ((fromIntegral f) `shiftL` 3) .|. w

decomposeFieldWire :: Word32 -> (FieldId,WireType)
decomposeFieldWire x = (FieldId (fromIntegral (x `shiftR` 3)), WireType (x .&. 7))

encodeWireMessage :: WireMessage -> ByteString
encodeWireMessage = undefined

decodeWireMessage :: ByteString -> WireMessage
decodeWireMessage = undefined

-} 