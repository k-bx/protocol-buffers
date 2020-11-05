{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Text.DescriptorProtos.FieldOptions.JSType (JSType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.List as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data JSType = JS_NORMAL
            | JS_STRING
            | JS_NUMBER
              deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable JSType

instance Prelude'.Bounded JSType where
  minBound = JS_NORMAL
  maxBound = JS_NUMBER

instance P'.Default JSType where
  defaultValue = JS_NORMAL

toMaybe'Enum :: Prelude'.Int -> P'.Maybe JSType
toMaybe'Enum 0 = Prelude'.Just JS_NORMAL
toMaybe'Enum 1 = Prelude'.Just JS_STRING
toMaybe'Enum 2 = Prelude'.Just JS_NUMBER
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum JSType where
  fromEnum JS_NORMAL = 0
  fromEnum JS_STRING = 1
  fromEnum JS_NUMBER = 2
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Text.DescriptorProtos.FieldOptions.JSType") .
      toMaybe'Enum
  succ JS_NORMAL = JS_STRING
  succ JS_STRING = JS_NUMBER
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Text.DescriptorProtos.FieldOptions.JSType"
  pred JS_STRING = JS_NORMAL
  pred JS_NUMBER = JS_STRING
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Text.DescriptorProtos.FieldOptions.JSType"

instance P'.Wire JSType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB JSType

instance P'.MessageAPI msg' (msg' -> JSType) JSType where
  getVal m' f' = f' m'

instance P'.ReflectEnum JSType where
  reflectEnum = [(0, "JS_NORMAL", JS_NORMAL), (1, "JS_STRING", JS_STRING), (2, "JS_NUMBER", JS_NUMBER)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".google.protobuf.FieldOptions.JSType") ["Text"] ["DescriptorProtos", "FieldOptions"] "JSType")
      ["Text", "DescriptorProtos", "FieldOptions", "JSType.hs"]
      [(0, "JS_NORMAL"), (1, "JS_STRING"), (2, "JS_NUMBER")]
      Prelude'.False

instance P'.TextType JSType where
  tellT = P'.tellShow
  getT = P'.getRead