{-# LANGUAGE RankNTypes #-}
-- everything passes version 0.2.7
-- ghci  -fcontext-stack=100 -XRankNTypes -XMultiParamTypeClasses  -XFlexibleInstances -isrc-auto-generated/ Arb/UnittestProto.hs

module Arb.UnittestProto where

import Arb
import qualified Manytat.ManyTAT as TAT
import qualified Manytat.ManyTR as TR

import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Numeric
import qualified System.Random as Random
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Gen(Gen(..))
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
import UnittestProto.TestRequired(TestRequired(..))

import UnittestProto
import UnittestProto.TestRequired
import UnittestProto.OptionalGroup_extension (OptionalGroup_extension(..))
import UnittestProto.RepeatedGroup_extension (RepeatedGroup_extension(..))
import UnittestProto.TestAllExtensions (TestAllExtensions(..))

import Debug.Trace(trace)

instance Arbitrary ImportEnum where arbitrary = barb
instance Arbitrary ForeignEnum where arbitrary = barb
instance Arbitrary NestedEnum where arbitrary = barb

instance Arbitrary ImportMessage where arbitrary = futz ImportMessage
instance Arbitrary ForeignMessage where arbitrary = futz ForeignMessage
instance Arbitrary NestedMessage where arbitrary = futz NestedMessage
instance Arbitrary OptionalGroup where arbitrary = futz OptionalGroup
instance Arbitrary RepeatedGroup where arbitrary = futz RepeatedGroup

instance Arbitrary TestRequired where arbitrary = futz TestRequired
--instance Arbitrary OptionalGroup_extension where arbitrary = futz OptionalGroup_extension
--instance Arbitrary RepeatedGroup_extension where arbitrary = futz RepeatedGroup_extension

instance Arbitrary TestAllTypes where 
  arbitrary = futz TestAllTypes

instance Arbitrary TestAllExtensions where
  arbitrary = F.foldlM (\msg alter -> alter msg) defaultValue (map snd allKeys)

-- Test passes up to 100000
check_SizeCalcTo limit = all prop_SizeCalc $ map NonNegative [0..limit]
prop_SizeCalc (NonNegative i) = prependMessageSize i == i + (L.length . runPut . putSize $ i)

-- quickCheck : TestAllTypes passes 100
prop_Size1 :: forall msg. (ReflectDescriptor msg, Wire msg) => msg -> Bool
prop_Size1 a =
  let predicted = messageSize a
      written = L.length (messagePut a)
  in if predicted == written then True
       else trace ("Wrong size: "++show (predicted,written)) False

-- quickCheck : TestAllTypes passes 100
prop_Size2 :: forall msg. (ReflectDescriptor msg, Wire msg) => msg -> Bool
prop_Size2 a =
  let predicted = messageWithLengthSize a
      written = L.length (messageWithLengthPut a)
  in if predicted == written then True
       else trace ("Wrong size: "++show(predicted,written)) False

-- convert with no header, a to a' compare a with a'
prop_WireArb1 :: (Show a,Eq a,Arbitrary a,ReflectDescriptor a,Wire a, Mergeable a) => a -> Bool
prop_WireArb1 a =
  case messageGet (messagePut a) of
     Right (a',b) | L.null b -> if da==a' then True
                                  else trace ("Unequal WireArb1\n" ++ show a ++ "\n\n" ++show a') False
                  | otherwise -> trace ("Not all input consumed: "++show (L.length b)++"\n"++ show a ++ "\n\n" ++show (L.unpack (messagePut a))) False
     Left msg -> trace (unlines [msg,show a,show . L.unpack $ messagePut a]) False
  where da = defaultValue `mergeAppend` a

type G x = Either String (x,ByteString)

-- convert with with header, a to a' compare a and a'
prop_WireArb2 :: (Eq a,Arbitrary a,ReflectDescriptor a,Wire a, Show a, Mergeable a) => a -> Bool
prop_WireArb2 a =
   case messageWithLengthGet (messageWithLengthPut a) of
     Right (a',b) | L.null b -> if da==a' then True
                                  else trace ("Unequal WireArb2\n" ++ show a ++ "\n\n" ++show a') False
                  | otherwise -> trace ("Not all input consumed: "++show (L.length b)) False
     Left msg -> trace msg False
  where da = defaultValue `mergeAppend` a

-- main method of serialing messages, aIn to a to a', compare a and a'
prop_WireArb3 :: (Show a,Eq a,Arbitrary a,ReflectDescriptor a,Wire a) => a -> Bool
prop_WireArb3 aIn =
   let unused = aIn==a
       Right (a,_) = messageGet (messagePut aIn) in
   case messageGet (messagePut a) of
     Right (a',b) | L.null b -> if a==a' then True
                                  else trace ("Unequal WireArb3\n" ++ show a ++ "\n\n" ++show a') False
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

-- Really push the extension system by creating two new keys here, one
-- for Maybe code and one for Seq code testing.
newOptKey :: Key Maybe TestAllExtensions Int32
newOptKey = Key 1000000 15 Nothing

newRepKey :: Key Seq TestAllExtensions Utf8
newRepKey = Key 1000001 9 Nothing

-- This is all 70 known for TestAllExtensions plus the two above.
-- The String names are currently discarded.
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
--  , ( "optionalGroup_extension" , maybeKey optionalGroup_extension )
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
 -- , ( "repeatedGroup_extension" , seqKey repeatedGroup_extension )
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
  [ ( "Size1" , prop_Size1 )
  , ( "Size2", prop_Size2 )
  , ( "WireArb1", prop_WireArb1 )
  , ( "WireArb2", prop_WireArb2 )
  , ( "WireArb3", prop_WireArb3 ) 
  ]


samples :: Gen a -> IO [a]
samples m =
  do rnd0 <- Random.newStdGen
     let --m' :: Random.StdGen -> (a,Random.StdGen)
         m' g k = let (g1,g2) = Random.split g
                  in (unGen m g1 k,g2)   -- reuse g, but what the hell
         rg g k = let (x,g') = m' g k in x : (rg g' $! succ k)
     return (rg rnd0 2)

cs = let a1= arbitrary :: Gen UnittestProto.TestAllTypes.TestAllTypes
     in do x <- sample' a1
           print (head x)
           x <- samples a1
           print (take 3 x)

makeFile outfile size = do
  let go s (x:xs) | s > size = mempty
                  | otherwise = let bs = messageWithLengthPut x
                                in mappend bs (go (s+L.length bs) xs)
  let a1 = arbitrary :: Gen UnittestProto.TestAllTypes.TestAllTypes
  xs <- samples a1
  L.writeFile outfile (go 0 xs)

makeManyTRFile outfile size = do
  let go s (x:xs) | s > size = []
                  | otherwise = x : go (s+messageWithLengthSize x) xs

  let a1 = arbitrary :: Gen UnittestProto.TestRequired.TestRequired
  xs <- samples a1
  let ys = go 0 xs
      manyTR :: TR.ManyTR
      manyTR = defaultValue { TR.tats = Seq.fromList ys }
  L.writeFile outfile (messagePut manyTR)


makeManyTATFile outfile size = do
  let go s (x:xs) | s > size = []
                  | otherwise = x : go (s+messageWithLengthSize x) xs

  let a1 = arbitrary :: Gen UnittestProto.TestAllTypes.TestAllTypes
  xs <- samples a1
  let ys = go 0 xs
      manyTAT :: TAT.ManyTAT
      manyTAT = defaultValue { TAT.tats = Seq.fromList ys }
  L.writeFile outfile (messagePut manyTAT)


