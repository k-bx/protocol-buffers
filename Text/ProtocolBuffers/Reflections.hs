-- To get the defaults working sanely, I need to encode at least some reflection information.
-- The to-be-bootstrapped descriptor.proto structures are not parsed enough for sane default usage.
-- So this is currently over-designed for the immediate need of DescriptorInfo -> number -> FieldInfo -> Maybe HsDefault.
-- These data structures and API are quite likely to be rewritten.
--
-- A strong feature of this is that it does not contain any structures defined by descriptor.proto!
-- This prevents me hitting any circular dependencies.
module Text.ProtocolBuffers.Reflections(ProtoName(..),DescriptorInfo(..),FieldInfo(..)
                                       ,HsDefault(..),EnumInfo(..),EnumInfoApp
                                       ,ReflectDescriptor(..),ReflectEnum(..)
                                       ,parseDefDouble,parseDefFloat
                                       ,parseDefBool,parseDefInteger
                                       ,parseDefString,parseDefBytes
                                       ,cEncode,cDecode
                                       ) where

import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString as BS (ByteString,null,pack,unpack)
import qualified Data.ByteString.Char8 as BSC(pack,unpack)
import Numeric
import Data.Char(ord,chr)
import Data.List(unfoldr)
import Data.Word(Word8)
import Data.Map(Map)
import Text.ProtocolBuffers.Basic
import Data.Generics(Data)
import Data.Typeable(Typeable)

data ProtoName = ProtoName { haskellPrefix :: String  -- Haskell specific prefix to module hierarchy (e.g. Text)
                           , parentModule :: String   -- Proto specified namespace (like java)
                           , baseName :: String       -- unqualfied name of this thing
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data DescriptorInfo = DescriptorInfo { descName :: ProtoName
                                     , fields :: Map MyInt32 FieldInfo }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data FieldInfo = FieldInfo { fieldName :: String
                           , fieldNumber :: MyInt32
                           , isRequired :: Bool
                           , canRepeat :: Bool
                           , typeCode :: Int                  -- ^ fromEnum of Text.DescriptorProtos.FieldDescriptorProto.Type
                           , typeName :: Maybe String
                           , hsRawDefault :: Maybe ByteString -- ^ crappy, perhaps escaped, thing
                           , hsDefault :: Maybe HsDefault     -- ^ nice parsed thing
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data HsDefault = HsDef'Bool Bool
               | HsDef'ByteString ByteString
               | HsDef'Rational Rational
               | HsDef'Integer Integer
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data EnumInfo = EnumInfo { enumName :: ProtoName
                         , enumItems :: [(MyInt32,String)]
                         }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

type EnumInfoApp e = [(MyInt32,String,e)]

class ReflectDescriptor m where
  reflectDescriptorInfo :: m -> DescriptorInfo        -- Must not inspect argument
  parentOfDescriptor :: m -> Maybe DescriptorInfo -- Must not inspect argument
  parentOfDescriptor _ = Nothing

class ReflectEnum e where
  reflectEnum :: EnumInfoApp e
  reflectEnumInfo :: e -> EnumInfo            -- Must not inspect argument
  parentOfEnum :: e -> Maybe DescriptorInfo   -- Must not inspect argument
  parentOfEnum _ = Nothing

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
parseDefBytes bs = Just . HsDef'ByteString 
                   . BS.pack . cDecode . BSC.unpack $ bs

cEncode :: [Word8] -> [Char]
cEncode = concatMap one where
  one 0 = ['\\','0']
  one 92 = ['\\','\\']
  one x | x > 127 = '\\':(showOct x "")
  one x = [toEnum .  fromEnum $  x]

cDecode :: [Char] -> [Word8]
cDecode = unfoldr one where
  one ('\\':'\\':xs) = Just (toEnum . fromEnum $ '\\',xs)
  one ('\\':'0':xs) = Just (0,xs)
  one ('\\':ys@(n:_:_:xs)) | '2'==n || '3' == n =
   case mayRead readOct (take 3 ys) of
     Just w -> Just (w,xs)
     Nothing -> Nothing
  one ('\\':_) = error "Text.ProtocolBuffers.Reflections.cDecode cannot understand your backslash escape value"
  one (x:xs) = Just (toEnum . fromEnum $ x,xs)
  one [] = Nothing

parseDefInteger :: ByteString -> Maybe HsDefault
parseDefInteger bs = fmap HsDef'Integer . mayRead checkSign . U.toString $ bs
    where checkSign = readSigned' checkBase
          checkBase ('0':'x':xs) = readHex xs
          checkBase ('0':xs) = readOct xs
          checkBase xs = readDec xs

readSigned' f ('-':xs) = map (\(v,s) -> (-v,s)) . f $ xs
readSigned' f ('+':xs) = f xs
readSigned' f xs = f xs

parseDefBool :: ByteString -> Maybe HsDefault
parseDefBool bs | bs == BSC.pack "true" = Just (HsDef'Bool True)
                | bs == BSC.pack "false" = Just (HsDef'Bool False)
                | otherwise = Nothing
