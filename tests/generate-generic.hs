{-# LANGUAGE RecordWildCards,RankNTypes,FlexibleInstances,MultiParamTypeClasses,DeriveDataTypeable #-}
module Main where

import Prelude hiding (mapM_)
import UnittestProto.TestAllTypes
import UnittestProto.TestAllExtensions
import Com.Google.Protobuf.Test.ImportEnum as Com.Google.Protobuf.Test (ImportEnum)
import Com.Google.Protobuf.Test.ImportMessage as Com.Google.Protobuf.Test (ImportMessage)
import UnittestProto.ForeignEnum as UnittestProto (ForeignEnum)
import UnittestProto.ForeignMessage as UnittestProto (ForeignMessage)
import UnittestProto.TestAllTypes.NestedEnum as UnittestProto.TestAllTypes (NestedEnum)
import UnittestProto.TestAllTypes.NestedMessage as UnittestProto.TestAllTypes (NestedMessage)
import UnittestProto.TestAllTypes.OptionalGroup as UnittestProto.TestAllTypes (OptionalGroup)
import UnittestProto.TestAllTypes.RepeatedGroup as UnittestProto.TestAllTypes (RepeatedGroup)

import qualified Data.ByteString.Lazy as L
import Control.Applicative((<$>),(<*>))

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.Get(runGetAll,Get,Result(..))
import Text.ProtocolBuffers.WireMessage(messageWithLengthGet)
import Text.ProtocolBuffers.Reflections
import qualified Data.Sequence as Seq
import Data.Foldable

joinMod :: [MName String] -> String
joinMod [] = ""
joinMod ms = Prelude.foldr1 (\a b -> a++"."++b) . map mName $ ms

qualFName :: ProtoFName -> String
qualFName p@(ProtoFName _ _prefix [] base) = fName base
qualFName p@(ProtoFName _ _prefix parents _base) = joinMod parents ++ "."++ (fName . baseName' $ p)

exts = do
  let kksOf :: ReflectDescriptor m => m -> [FieldInfo]
      kksOf = toList . knownKeys . reflectDescriptorInfo
      fn = qualFName . fieldName
      see fi@(FieldInfo {packedTag=Just (WireTag wt1, WireTag wt2),..}) =
        if isPacked then "packedKey "++fn fi
          else "unpackedKey "++fn fi
      see fi@(FieldInfo {..}) | canRepeat = "repKey "++fn fi
                              | otherwise = "optKey "++fn fi
      format s = ", P'."++s
  print (mName . baseName . descName . reflectDescriptorInfo $ (undefined :: TestAllExtensions))
  mapM_ (putStrLn . format . see) $ kksOf (undefined :: TestAllExtensions)

main = do
  let fisOf :: ReflectDescriptor m => m -> [FieldInfo]
      fisOf = toList . fields . reflectDescriptorInfo
      pre (FieldInfo {..}) = if canRepeat then "rep1" else (if isRequired then "req" else "opt")
      pre1 (FieldInfo {..}) = if canRepeat then "rep" else (if isRequired then "req" else "opt")
      see (FieldInfo {packedTag=Just (WireTag wt1, WireTag wt2),..}) =
        "rep2Primitive "++show wt1++" "++show wt2++" "++show (getFieldType typeCode)
      see fi@(FieldInfo {..}) | typeCode==10 =
        (pre fi)++"Group "++show (getWireTag wireTag)
      see fi@(FieldInfo {..}) | typeCode==11 =
        (pre fi)++"Message "++show (getWireTag wireTag)
                              | otherwise =
        (pre1 fi)++"Primitive "++show (getWireTag wireTag)++" "++show (getFieldType typeCode)

      see1 (FieldInfo {..}) = ( fName . baseName' $ fieldName
                             , if isRequired then "req"
                                 else if canRepeat then "rep_"
                                     else "opt"
                             , if 11 == typeCode then "Message"
                                 else if 10 == typeCode then "Group"
                                        else "Primitive"
                             , maybe (Right . getWireTag $ wireTag) Left packedTag
                             , getFieldType $ typeCode
                             )
      format s = "<*> P'."++s++" g'"
  print (mName . baseName . descName . reflectDescriptorInfo $ (undefined :: TestAllTypes))
  mapM_ (putStrLn . format . see) $ fisOf (undefined :: TestAllTypes)

  print (mName . baseName . descName . reflectDescriptorInfo $ (undefined :: NestedMessage))
  mapM_ (putStrLn . format . see) $ fisOf (undefined :: NestedMessage)

  print (mName . baseName . descName . reflectDescriptorInfo $ (undefined :: ForeignMessage))
  mapM_ (putStrLn . format . see) $ fisOf (undefined :: ForeignMessage)

  print (mName . baseName . descName . reflectDescriptorInfo $ (undefined :: ImportMessage))
  mapM_ (putStrLn . format . see) $ fisOf (undefined :: ImportMessage)

  print (mName . baseName . descName . reflectDescriptorInfo $ (undefined :: OptionalGroup))
  mapM_ (putStrLn . format . see) $ fisOf (undefined :: OptionalGroup)

  print (mName . baseName . descName . reflectDescriptorInfo $ (undefined :: RepeatedGroup))
  mapM_ (putStrLn . format . see) $ fisOf (undefined :: RepeatedGroup)

test = do
  f <- L.readFile "tat.bin"
  let r = runGetAll (getMessage :: Get TestAllTypes) f
  let r2 = messageWithLengthGet f :: Either String (TestAllTypes,ByteString)
  putStrLn (case r of
             Failed {} -> show r
             Finished _ _ r' -> show r'
             Partial {} -> "wtf Partial")
  putStrLn (either id (show . fst) r2)

