{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Text.DescriptorProtos.FileOptions.OptimizeMode (OptimizeMode(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data OptimizeMode = SPEED
                  | CODE_SIZE
                  | LITE_RUNTIME
                  deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable OptimizeMode
 
instance Prelude'.Bounded OptimizeMode where
  minBound = SPEED
  maxBound = LITE_RUNTIME
 
instance P'.Default OptimizeMode where
  defaultValue = SPEED
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe OptimizeMode
toMaybe'Enum 1 = Prelude'.Just SPEED
toMaybe'Enum 2 = Prelude'.Just CODE_SIZE
toMaybe'Enum 3 = Prelude'.Just LITE_RUNTIME
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum OptimizeMode where
  fromEnum SPEED = 1
  fromEnum CODE_SIZE = 2
  fromEnum LITE_RUNTIME = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Text.DescriptorProtos.FileOptions.OptimizeMode")
      . toMaybe'Enum
  succ SPEED = CODE_SIZE
  succ CODE_SIZE = LITE_RUNTIME
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Text.DescriptorProtos.FileOptions.OptimizeMode"
  pred CODE_SIZE = SPEED
  pred LITE_RUNTIME = CODE_SIZE
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Text.DescriptorProtos.FileOptions.OptimizeMode"
 
instance P'.Wire OptimizeMode where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB OptimizeMode
 
instance P'.MessageAPI msg' (msg' -> OptimizeMode) OptimizeMode where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum OptimizeMode where
  reflectEnum = [(1, "SPEED", SPEED), (2, "CODE_SIZE", CODE_SIZE), (3, "LITE_RUNTIME", LITE_RUNTIME)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".google.protobuf.FileOptions.OptimizeMode") ["Text"] ["DescriptorProtos", "FileOptions"] "OptimizeMode")
      ["Text", "DescriptorProtos", "FileOptions", "OptimizeMode.hs"]
      [(1, "SPEED"), (2, "CODE_SIZE"), (3, "LITE_RUNTIME")]