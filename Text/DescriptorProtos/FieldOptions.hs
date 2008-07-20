-- *Text.ProtocolBuffers.Gen Numeric> putStrLn . prettyPrint . descriptorModule "Text" $ genFieldOptions
module Text.DescriptorProtos.FieldOptions (FieldOptions(..)) where
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FieldOptions.CType
       as DescriptorProtos.FieldOptions (CType)
 
data FieldOptions = FieldOptions{ctype ::
                                 P'.Maybe DescriptorProtos.FieldOptions.CType,
                                 experimental_map_key :: P'.Maybe P'.ByteString}
                  deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable FieldOptions where
        mergeEmpty = FieldOptions P'.mergeEmpty P'.mergeEmpty
        mergeAppend (FieldOptions x'1 x'2) (FieldOptions y'1 y'2)
          = FieldOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default FieldOptions where
        defaultValue
          = FieldOptions (P'.Just P'.defaultValue) (P'.Just P'.defaultValue)
 
instance P'.ReflectDescriptor FieldOptions where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FieldOptions\"}, fields = fromList [(1,FieldInfo {fieldName = \"ctype\", fieldNumber = 1, isRequired = False, canRepeat = False, typeCode = 10, typeName = Just \"DescriptorProtos.FieldOptions.CType\", hsRawDefault = Nothing, hsDefault = Nothing}),(9,FieldInfo {fieldName = \"experimental_map_key\", fieldNumber = 9, isRequired = False, canRepeat = False, typeCode = 8, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing})]}"

{-module Text.DescriptorProtos.FieldOptions
  (FieldOptions(..))
 where

import Text.ProtocolBuffers.Header

import qualified Text.DescriptorProtos.FieldOptions.CType as DescriptorProtos.FieldOptions(CType)

data FieldOptions = FieldOptions
    { ctype :: Maybe DescriptorProtos.FieldOptions.CType
    , experimental_map_key :: Maybe ByteString
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''FieldOptions )

instance Default FieldOptions where

-}