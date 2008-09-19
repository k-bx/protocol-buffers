module Arb.UnittestProto.TestAllTypes where

import Arb

import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Numeric
import Test.QuickCheck
import Text.ProtocolBuffers
import Text.ProtocolBuffers.Header(prependMessageSize,putSize)

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.WireMessage
import Text.ProtocolBuffers.Extensions

import Com.Google.Protobuf.Test.ImportEnum(ImportEnum(..))
import Com.Google.Protobuf.Test.ImportMessage(ImportMessage(..))
import UnittestProto.ForeignEnum(ForeignEnum(..))
import UnittestProto.ForeignMessage(ForeignMessage(..))
import UnittestProto.TestAllTypes.NestedEnum(NestedEnum(..))
import UnittestProto.TestAllTypes.NestedMessage(NestedMessage(..))
import UnittestProto.TestAllTypes.OptionalGroup(OptionalGroup(..))
import UnittestProto.TestAllTypes.RepeatedGroup(RepeatedGroup(..))

import UnittestProto.TestAllTypes(TestAllTypes(..))

import UnittestProto
import UnittestProto.TestRequired
import UnittestProto.OptionalGroup_extension (OptionalGroup_extension(..))
import UnittestProto.RepeatedGroup_extension (RepeatedGroup_extension(..))
import UnittestProto.TestAllExtensions (TestAllExtensions(..))

import Debug.Trace(trace)

barb :: (Enum a,Bounded a) => Gen a
barb = elements [minBound..maxBound]

instance Arbitrary ImportEnum where arbitrary = barb
instance Arbitrary ForeignEnum where arbitrary = barb
instance Arbitrary NestedEnum where arbitrary = barb

class ArbCon a x where
  futz :: a -> Gen x

instance ArbCon a a where futz = return

instance (Arbitrary a,ArbCon b x) => ArbCon (a -> b) x where
  futz f = arbitrary >>= futz . f

instance Arbitrary ImportMessage where arbitrary = futz ImportMessage
instance Arbitrary ForeignMessage where arbitrary = futz ForeignMessage
instance Arbitrary NestedMessage where arbitrary = futz NestedMessage
instance Arbitrary OptionalGroup where arbitrary = futz OptionalGroup
instance Arbitrary RepeatedGroup where arbitrary = futz RepeatedGroup
instance Arbitrary TestAllTypes where arbitrary = futz TestAllTypes

instance Arbitrary TestRequired where arbitrary = futz TestRequired
instance Arbitrary OptionalGroup_extension where arbitrary = futz OptionalGroup_extension
instance Arbitrary RepeatedGroup_extension where arbitrary = futz RepeatedGroup_extension

instance Arbitrary TestAllExtensions where
  arbitrary = F.foldlM (\msg alter -> alter msg) defaultValue (map snd allKeys)

-- Test passes up to 100000
prop_SizeCalcTo limit = all prop_SizeCalc [0..limit] where
  prop_SizeCalc i = prependMessageSize i == i + (L.length . runPut . putSize $ i)

-- quickCheck : TestAllTypes passes 100
prop_Size1 :: forall msg. (ReflectDescriptor msg, Wire msg) => msg -> Bool
prop_Size1 a =
  let predicted = messageSize a
      written = L.length (messagePut a)
  in if predicted == written then True
       else trace ("Wrong size: "++show(predicted,written)) False

-- quickCheck : TestAllTypes passes 100
prop_Size2 :: forall msg. (ReflectDescriptor msg, Wire msg) => msg -> Bool
prop_Size2 a =
  let predicted = messageWithLengthSize a
      written = L.length (messageWithLengthPut a)
  in if predicted == written then True
       else trace ("Wrong size: "++show(predicted,written)) False

-- convert with no header
prop_WireArb1 :: (Show a,Eq a,Arbitrary a,ReflectDescriptor a,Wire a) => a -> Bool
prop_WireArb1 a =
   case messageGet (messagePut a) of
     Right (a',b) | L.null b -> if a==a' then True
                                  else trace ("Unequal\n" ++ show a ++ "\n\n" ++show a') False
                  | otherwise -> trace ("Not all input consumed: "++show (L.length b)++"\n"++ show a ++ "\n\n" ++show (L.unpack (messagePut a))) False
     Left msg -> trace msg False

type G x = Either String (x,ByteString)

prop_WireArb3 :: (Show a,Eq a,Arbitrary a,ReflectDescriptor a,Wire a) => a -> Bool
prop_WireArb3 aIn =
   let unused = aIn==a
       Right (a,_) = messageGet (messagePut aIn) in
   case messageGet (messagePut a) of
     Right (a',b) | L.null b -> if a==a' then True
                                  else trace ("Unequal\n" ++ show a ++ "\n\n" ++show a') False
                  | otherwise -> trace ("Not all input consumed: "++show (L.length b)) False
     Left msg -> trace msg False

-- convert with with header
prop_WireArb2 :: (Eq a,Arbitrary a,ReflectDescriptor a,Wire a) => a -> Bool
prop_WireArb2 a =
   case messageWithLengthGet (messageWithLengthPut a) of
     Right (a',b) | L.null b -> if a==a' then True
                                  else trace ("Unequal") False
                  | otherwise -> trace ("Not all input consumed: "++show (L.length b)) False
     Left msg -> trace msg False

-- used in allKeys
maybeKey :: Arbitrary v => Key Maybe msg v -> msg -> Gen msg
maybeKey k = \msg -> do
  b <- choose (False,True)
  if b then return msg
    else do
  v <- arbitrary
  return (putExt k (Just v) msg)

-- used in allKeys
seqKey :: Arbitrary v => Key Seq msg v -> msg -> Gen msg
seqKey k = \msg -> do
  n <- choose (0,3)
  v <- vector n
  return (putExt k (Seq.fromList v) msg)

newOptKey :: Key Maybe TestAllExtensions Int32
newOptKey = Key 1000000 15 Nothing

newRepKey :: Key Seq TestAllExtensions Utf8
newRepKey = Key 1000001 9 Nothing

-- This is all 70 known for TestAllExtensions
allKeys :: [ ( String , TestAllExtensions -> Gen TestAllExtensions ) ]
allKeys = 
  [ ( "newOptKey" , maybeKey newOptKey )
  , ( "newRepKey" , seqKey newRepKey )
  , ( "single" , maybeKey single )
  , ( "multi" , seqKey multi )
  , ( "optional_int32_extension" , maybeKey optional_int32_extension )
  , ( "optional_int64_extension" , maybeKey optional_int64_extension )
  , ( "optional_uint32_extension" , maybeKey optional_uint32_extension )
  , ( "optional_uint64_extension" , maybeKey optional_uint64_extension )
  , ( "optional_sint32_extension" , maybeKey optional_sint32_extension )
  , ( "optional_sint64_extension" , maybeKey optional_sint64_extension )
  , ( "optional_fixed32_extension" , maybeKey optional_fixed32_extension )
  , ( "optional_fixed64_extension" , maybeKey optional_fixed64_extension )
  , ( "optional_sfixed32_extension" , maybeKey optional_sfixed32_extension )
  , ( "optional_sfixed64_extension" , maybeKey optional_sfixed64_extension )
  , ( "optional_float_extension" , maybeKey optional_float_extension )
  , ( "optional_double_extension" , maybeKey optional_double_extension )
  , ( "optional_bool_extension" , maybeKey optional_bool_extension )
  , ( "optional_string_extension" , maybeKey optional_string_extension )
  , ( "optional_bytes_extension" , maybeKey optional_bytes_extension )
  , ( "optionalGroup_extension" , maybeKey optionalGroup_extension )
  , ( "optional_nested_message_extension" , maybeKey optional_nested_message_extension )
  , ( "optional_foreign_message_extension" , maybeKey optional_foreign_message_extension )
  , ( "optional_import_message_extension" , maybeKey optional_import_message_extension )
  , ( "optional_nested_enum_extension" , maybeKey optional_nested_enum_extension )
  , ( "optional_foreign_enum_extension" , maybeKey optional_foreign_enum_extension )
  , ( "optional_import_enum_extension" , maybeKey optional_import_enum_extension )
  , ( "optional_string_piece_extension" , maybeKey optional_string_piece_extension )
  , ( "optional_cord_extension" , maybeKey optional_cord_extension )
  , ( "repeated_int32_extension" , seqKey repeated_int32_extension )
  , ( "repeated_int64_extension" , seqKey repeated_int64_extension )
  , ( "repeated_uint32_extension" , seqKey repeated_uint32_extension )
  , ( "repeated_uint64_extension" , seqKey repeated_uint64_extension )
  , ( "repeated_sint32_extension" , seqKey repeated_sint32_extension )
  , ( "repeated_sint64_extension" , seqKey repeated_sint64_extension )
  , ( "repeated_fixed32_extension" , seqKey repeated_fixed32_extension )
  , ( "repeated_fixed64_extension" , seqKey repeated_fixed64_extension )
  , ( "repeated_sfixed32_extension" , seqKey repeated_sfixed32_extension )
  , ( "repeated_sfixed64_extension" , seqKey repeated_sfixed64_extension )
  , ( "repeated_float_extension" , seqKey repeated_float_extension )
  , ( "repeated_double_extension" , seqKey repeated_double_extension )
  , ( "repeated_bool_extension" , seqKey repeated_bool_extension )
  , ( "repeated_string_extension" , seqKey repeated_string_extension )
  , ( "repeated_bytes_extension" , seqKey repeated_bytes_extension )
  , ( "repeatedGroup_extension" , seqKey repeatedGroup_extension )
  , ( "repeated_nested_message_extension" , seqKey repeated_nested_message_extension )
  , ( "repeated_foreign_message_extension" , seqKey repeated_foreign_message_extension )
  , ( "repeated_import_message_extension" , seqKey repeated_import_message_extension )
  , ( "repeated_nested_enum_extension" , seqKey repeated_nested_enum_extension )
  , ( "repeated_foreign_enum_extension" , seqKey repeated_foreign_enum_extension )
  , ( "repeated_import_enum_extension" , seqKey repeated_import_enum_extension )
  , ( "repeated_string_piece_extension" , seqKey repeated_string_piece_extension )
  , ( "repeated_cord_extension" , seqKey repeated_cord_extension )
  , ( "default_int32_extension" , maybeKey default_int32_extension )
  , ( "default_int64_extension" , maybeKey default_int64_extension )
  , ( "default_uint32_extension" , maybeKey default_uint32_extension )
  , ( "default_uint64_extension" , maybeKey default_uint64_extension )
  , ( "default_sint32_extension" , maybeKey default_sint32_extension )
  , ( "default_sint64_extension" , maybeKey default_sint64_extension )
  , ( "default_fixed32_extension" , maybeKey default_fixed32_extension )
  , ( "default_fixed64_extension" , maybeKey default_fixed64_extension )
  , ( "default_sfixed32_extension" , maybeKey default_sfixed32_extension )
  , ( "default_sfixed64_extension" , maybeKey default_sfixed64_extension )
  , ( "default_float_extension" , maybeKey default_float_extension )
  , ( "default_double_extension" , maybeKey default_double_extension )
  , ( "default_bool_extension" , maybeKey default_bool_extension )
  , ( "default_string_extension" , maybeKey default_string_extension )
  , ( "default_bytes_extension" , maybeKey default_bytes_extension )
  , ( "default_nested_enum_extension" , maybeKey default_nested_enum_extension )
  , ( "default_foreign_enum_extension" , maybeKey default_foreign_enum_extension )
  , ( "default_import_enum_extension" , maybeKey default_import_enum_extension )
  , ( "default_string_piece_extension" , maybeKey default_string_piece_extension )
  , ( "default_cord_extension" , maybeKey default_cord_extension )
  ]

tests_TestAllTypes :: [(String,TestAllTypes -> Bool)]
tests_TestAllTypes =
 [ ( "Size1" , prop_Size1 )
 , ( "Size2", prop_Size2 )
 , ( "WireArb1", prop_WireArb1 )
 , ( "WireArb2", prop_WireArb2 )
 ]


tests_TestAllExtensions :: [(String,TestAllExtensions -> Bool)]
tests_TestAllExtensions =
 [ ( "WireArb1", prop_WireArb1 )
 , ( "WireArb2", prop_WireArb2 )
 , ( "Size1" , prop_Size1 )
 , ( "Size2", prop_Size2 )
 , ( "WireArb3", prop_WireArb3 )
 ]

main =  do
  mapM_ (\(name,test) -> putStrLn name >> quickCheck test) tests_TestAllTypes
  mapM_ (\(name,test) -> putStrLn name >> quickCheck test) tests_TestAllExtensions

{-
WireArb1
Not all input consumed: 24
TestAllExtensions {ext'field = ExtField (fromList [(FieldId {getFieldId = 31},ExtRepeated (FieldType {getFieldType = 5}) (GPDynSeq (GPWitness :: GPWitness (Int32)) (fromList [-1397484008,-2013522346,638582067]))),(FieldId {getFieldId = 1000000},ExtOptional (FieldType {getFieldType = 15}) (GPDyn (GPWitness :: GPWitness (Int32)) (1586669532))),(FieldId {getFieldId = 1000001},ExtRepeated (FieldType {getFieldType = 9}) (GPDynSeq (GPWitness :: GPWitness (Text.ProtocolBuffers.Basic.Utf8)) (fromList [Utf8 {utf8 = Chunk "\242\178\158\184\225\169\173" Empty},Utf8 {utf8 = Chunk "\232\133\138\243\150\188\172" Empty}])))])}

[248,1,152,172,208,229,10,248,1,214,172,240,191,8,248,1,179,250,191,176,2,133,164,232,3,220,167,146,94,138,164,232,3,7,242,178,158,184,225,169,173,138,164,232,3,7,232,133,138,243,150,188,172]
-}

bad :: ByteString
bad = L.pack bytes

bytes = bytes1++bytes2++bytes3
bytes1 = [248,1,152,172,208,229,10,248,1,214,172,240,191,8,248,1,179,250,191,176,2,133,164,232,3,220,167,146,94]
bytes2 = [138,164,232,3,7,242,178,158,184,225,169,173]
bytes3 = [138,164,232,3,7,232,133,138,243,150,188,172]

fails = messageGet bad :: G TestAllExtensions
front = messageGet (L.take (L.length bad - 24) bad) :: G TestAllExtensions
back1 = messageGet (L.drop (L.length bad - 24) bad) :: G TestAllExtensions
back2 = messageGet (L.drop (L.length bad - 12) bad) :: G TestAllExtensions

wiretag :: Int64
wiretag = (138-128)+(164-128)*128+(232-128)*128*128+3*128*128*128

(fi,wt) = quotRem wiretag 8

