-- To get the defaults working sanely, I need to encode at least some reflection information.
-- The to-be-bootstrapped descriptor.proto structures are not parsed enough for sane default usage.
-- So this is currently over-designed for the immediate need of DescriptorInfo -> number -> FieldInfo -> Maybe HsDefault.
-- These data structures and API are quite likely to be rewritten.
--
-- A strong feature of this is that it does not contain any structures defined by descriptor.proto!
-- This prevents me hitting any circular dependencies.
--
-- 
module Text.ProtocolBuffers.Reflections(ProtoName(..),DescriptorInfo(..),FieldInfo(..),KeyInfo
                                       ,HsDefault(..),EnumInfo(..),EnumInfoApp
                                       ,ReflectDescriptor(..),ReflectEnum(..),GetMessageInfo(..)
                                       ,parseDefDouble,parseDefFloat
                                       ,parseDefBool,parseDefInteger
                                       ,parseDefString,parseDefBytes
--                                       ,cEncode,cDecode
                                       ) where

import Text.ProtocolBuffers.Basic

import qualified Data.ByteString.Lazy.UTF8 as U(toString,fromString)
import Numeric(readHex,readOct,readDec,showOct)
import Data.Char(ord,chr,isHexDigit,isOctDigit,toLower)
import Data.List(sort,unfoldr)
import qualified Data.Foldable as F(toList)
import Data.Bits(Bits((.|.),shiftL))
import Data.Word(Word8)
import Data.Set(Set)
import qualified Data.Set as Set(fromDistinctAscList)
import Data.Generics(Data)
import Data.Typeable(Typeable)
import Test.QuickCheck(quickCheck)
import Codec.Binary.UTF8.String(encode)

data ProtoName = ProtoName { haskellPrefix :: String  -- Haskell specific prefix to module hierarchy (e.g. Text)
                           , parentModule :: String   -- Proto specified namespace (like java)
                           , baseName :: String       -- unqualfied name of this thing
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data DescriptorInfo = DescriptorInfo { descName :: ProtoName
                                     , fields :: Seq FieldInfo 
                                     , keys :: Seq KeyInfo
                                     , extRanges :: [(FieldId,FieldId)]
                                     }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data GetMessageInfo = GetMessageInfo { requiredTags :: Set WireTag
                                     , allowedTags :: Set WireTag
                                     }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

type KeyInfo = (String,FieldInfo)

data FieldInfo = FieldInfo { fieldName :: String
                           , fieldNumber :: FieldId
                           , wireTag :: WireTag
                           , wireTagLength :: Int64           -- ^ Bytes required in the Varint formatted wireTag
                           , isRequired :: Bool
                           , canRepeat :: Bool
                           , typeCode :: FieldType            -- ^ fromEnum of Text.DescriptorProtos.FieldDescriptorProto.Type
                           , typeName :: Maybe String         -- XXX remove Maybe and call useType?
                           , hsRawDefault :: Maybe ByteString -- ^ crappy, but not escaped, thing
                           , hsDefault :: Maybe HsDefault     -- ^ nice parsed thing
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

-- | 'HsDefault' stores the parsed default from the proto file in a
-- form that will make a nice literal in the
-- Language.Haskell.Exts.Syntax sense.
data HsDefault = HsDef'Bool Bool
               | HsDef'ByteString ByteString
               | HsDef'Rational Rational
               | HsDef'Integer Integer
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data EnumInfo = EnumInfo { enumName :: ProtoName
                         , enumItems :: [(EnumCode,String)]
                         }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

type EnumInfoApp e = [(EnumCode,String,e)]

class ReflectDescriptor m where
  getMessageInfo :: m -> GetMessageInfo           -- Must not inspect argument
  getMessageInfo x = cached where cached = makeMessageInfo (reflectDescriptorInfo (undefined `asTypeOf` x))
  reflectDescriptorInfo :: m -> DescriptorInfo    -- Must not inspect argument
  parentOfDescriptor :: m -> Maybe DescriptorInfo -- Must not inspect argument
  parentOfDescriptor _ = Nothing

class ReflectEnum e where
  reflectEnum :: EnumInfoApp e
  reflectEnumInfo :: e -> EnumInfo            -- Must not inspect argument
  parentOfEnum :: e -> Maybe DescriptorInfo   -- Must not inspect argument
  parentOfEnum _ = Nothing

makeMessageInfo :: DescriptorInfo -> GetMessageInfo
makeMessageInfo di = GetMessageInfo { requiredTags = Set.fromDistinctAscList . sort $
                                        [ wireTag f | f <- F.toList (fields di), isRequired f]
                                    , allowedTags = Set.fromDistinctAscList . sort $
                                        [ wireTag f | f <- F.toList (fields di)]
                                    }

--- From here down is code used to parse the format of the default values in the .proto files

{-# INLINE mayRead #-}
mayRead :: ReadS a -> String -> Maybe a
mayRead f s = case f s of [(a,"")] -> Just a; _ -> Nothing

parseDefDouble :: ByteString -> Maybe HsDefault
parseDefDouble bs = fmap (HsDef'Rational . toRational) 
                    . mayRead reads' . U.toString $ bs
  where reads' :: ReadS Double
        reads' = readSigned' reads

parseDefFloat :: ByteString -> Maybe HsDefault
parseDefFloat bs = fmap  (HsDef'Rational . toRational) 
                   . mayRead reads' . U.toString $ bs
  where reads' :: ReadS Float
        reads' = readSigned' reads

parseDefString :: ByteString -> Maybe HsDefault
parseDefString bs = Just (HsDef'ByteString bs)

parseDefBytes :: ByteString -> Maybe HsDefault
parseDefBytes bs = Just (HsDef'ByteString bs)

parseDefInteger :: ByteString -> Maybe HsDefault
parseDefInteger bs = fmap HsDef'Integer . mayRead checkSign . U.toString $ bs
    where checkSign = readSigned' checkBase
          checkBase ('0':'x':xs) = readHex xs
          checkBase ('0':xs) = readOct xs
          checkBase xs = readDec xs

parseDefBool :: ByteString -> Maybe HsDefault
parseDefBool bs | bs == U.fromString "true" = Just (HsDef'Bool True)
                | bs == U.fromString "false" = Just (HsDef'Bool False)
                | otherwise = Nothing

-- The Numeric.readSigned does not handle '+' for some odd reason
readSigned' f ('-':xs) = map (\(v,s) -> (-v,s)) . f $ xs
readSigned' f ('+':xs) = f xs
readSigned' f xs = f xs

{-

-- see google's stubs/strutil.cc lines 398-449/1121 and C99 specification
-- This mainly targets three digit octal codes
cEncode :: [Word8] -> [Char]
cEncode = concatMap one where
  one :: Word8 -> [Char]
  one x | (32 <= x) && (x < 127) = [toEnum .  fromEnum $  x]  -- main case of unescaped value
  one 9 = sl  't'
  one 10 = sl 'n'
  one 13 = sl 'r'
  one 34 = sl '"'
  one 39 = sl '\''
  one 92 = sl '\\'
  one 0 = '\\':"000"
  one x | x < 8 = '\\':'0':'0':(showOct x "")
        | x < 64 = '\\':'0':(showOct x "")
        | otherwise = '\\':(showOct x "")
  sl c = ['\\',c]

-- Takes backslash encoded junk an retrieves the underlying bytes
-- The \x is kosher, but \X is non-standard.  I recognize \X because google does
-- Convert backslash-escaped \u and \U crap into UTF-8 (this is why we have to use concat!)
-- It only works on characters and values in the range 0..255 (the Char8's returned by ByteString.Char.unpack)
-- It ONLY consumes up to 3 octal digits when reading an escape sequence, the rest are left alone.
cDecode :: [Char] -> [Word8]
cDecode = concat . unfoldr one where
  one :: [Char] -> Maybe ([Word8],[Char])
  one (x:xs) | x /= '\\' = Just (checkChar8 x,xs)  -- main case of unescaped value
  one [] = Nothing
  one ('\\':[]) = error "Text.ProtocolBuffers.Reflections.cDecode cannot understand ending with a backslash"
  one ('\\':ys) | 1 <= len =
      case mayRead readOct oct of
        Just w -> Just (checkByte w,rest)
        Nothing -> error "Text.ProtocolBuffers.Reflections.cDecode failed to decode octal sequence"
    where oct = takeWhile isOctDigit (take 3 ys)
          len = length oct
          rest = drop len ys
  one ('\\':x:ys) | 'x' == toLower x && ok =
      case mayRead readHex hex of
        Just w -> Just (checkByte w,rest)
        Nothing -> error "Text.ProtocolBuffers.Reflections.cDecode failed to decode hex sequence"
    where ok = 1 <= length hex
          (hex,rest) = span isHexDigit ys
  one ('\\':'u':ys) | ok =
      case mayRead readHex hex of
        Just w -> Just (checkUnicode w,rest)
        Nothing -> error "Text.ProtocolBuffers.Reflections.cDecode failed to decode 4 char unicode sequence"
    where ok = all isHexDigit hex && 4 == length hex
          (hex,rest) = splitAt 4 ys
  one ('\\':'U':ys) | ok =
      case mayRead readHex hex of
        Just w -> Just (checkUnicode w,rest)
        Nothing -> error "Text.ProtocolBuffers.Reflections.cDecode failed to decode 8 char unicode sequence"
    where ok = all isHexDigit hex && 8 == length hex
          (hex,rest) = splitAt 8 ys
  one ('\\':(x:xs)) = Just ([decode x],xs)
  decode :: Char -> Word8
  decode 'a' = 7
  decode 'b' = 8
  decode 't' = 9
  decode 'n' = 10
  decode 'v' = 11
  decode 'f' = 12
  decode 'r' = 13
  decode '\"' = 34
  decode '\'' = 39
  decode '?' = 63    -- C99 rule : "\?" is '?'
  decode '\\' = 92
  decode x | toLower x == 'x' = error "Text.ProtocolBuffers.Reflections.cDecode cannot understand your 'xX' hexadecimal escaped value"
  decode x | toLower x == 'u' = error "Text.ProtocolBuffers.Reflections.cDecode cannot understand your 'uU' unicode UTF-8 hexadecimal escaped value"
  decode _ = error "Text.ProtocolBuffers.Reflections.cDecode cannot understand your backslash-escaped value"
  checkChar8 :: Char -> [Word8]
  checkChar8 c | (0 <= i) && (i <= 255) = [toEnum i]
               | otherwise = error "Text.ProtocolBuffers.Reflections.cDecode found Char out of range 0..255"
    where i = fromEnum c
  checkByte :: Integer -> [Word8]
  checkByte i | (0 <= i) && (i <= 255) = [fromInteger i]
              | otherwise = error "Text.ProtocolBuffers.Reflections.cDecode found Oct/Hex Int out of range 0..255"
  checkUnicode :: Integer -> [Word8]
  checkUnicode i | (0 <= i) && (i <= 127) = [fromInteger i]
                 | i <= maxChar = encode [ toEnum . fromInteger $ i ]
                 | otherwise = error "Text.ProtocolBuffers.Reflections.cDecode found Unicode Char out of range 0..0x10FFFF"
    where maxChar = toInteger (fromEnum (maxBound ::Char)) -- 0x10FFFF

testEncodeDecode = let q :: [Int] -> Bool
                       q =  (\y -> let x = map (\z->abs z `mod` 255) y
                                   in  x == (map fromEnum . cDecode . cEncode . map toEnum$ x))
                   in quickCheck q
-}