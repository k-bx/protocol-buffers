module UnittestProto.TestExtremeDefaultValues (TestExtremeDefaultValues(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data TestExtremeDefaultValues = TestExtremeDefaultValues{escaped_bytes :: P'.Maybe P'.ByteString,
                                                         large_uint32 :: P'.Maybe P'.Word32, large_uint64 :: P'.Maybe P'.Word64,
                                                         small_int32 :: P'.Maybe P'.Int32, small_int64 :: P'.Maybe P'.Int64,
                                                         utf8_string :: P'.Maybe P'.Utf8, unknown'field :: P'.UnknownField}
                              deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage TestExtremeDefaultValues where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable TestExtremeDefaultValues where
  mergeEmpty
    = TestExtremeDefaultValues P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (TestExtremeDefaultValues x'1 x'2 x'3 x'4 x'5 x'6 x'7) (TestExtremeDefaultValues y'1 y'2 y'3 y'4 y'5 y'6 y'7)
    = TestExtremeDefaultValues (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
        (P'.mergeAppend x'5 y'5)
        (P'.mergeAppend x'6 y'6)
        (P'.mergeAppend x'7 y'7)
 
instance P'.Default TestExtremeDefaultValues where
  defaultValue
    = TestExtremeDefaultValues (P'.Just (P'.pack "\NUL\SOH\a\b\f\n\r\t\v\\'\"\254")) (P'.Just 4294967295)
        (P'.Just 18446744073709551615)
        (P'.Just (-2147483647))
        (P'.Just (-9223372036854775807))
        (P'.Just (P'.Utf8 (P'.pack "\225\136\180")))
        P'.defaultValue
 
instance P'.Wire TestExtremeDefaultValues where
  wireSize ft' self'@(TestExtremeDefaultValues x'1 x'2 x'3 x'4 x'5 x'6 x'7)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
          = (P'.wireSizeOpt 1 12 x'1 + P'.wireSizeOpt 1 13 x'2 + P'.wireSizeOpt 1 4 x'3 + P'.wireSizeOpt 1 5 x'4 +
               P'.wireSizeOpt 1 3 x'5
               + P'.wireSizeOpt 1 9 x'6
               + P'.wireSizeUnknownField x'7)
  wirePut ft' self'@(TestExtremeDefaultValues x'1 x'2 x'3 x'4 x'5 x'6 x'7)
    = case ft' of
        10 -> put'Fields
        11
          -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
        _ -> P'.wirePutErr ft' self'
    where
        put'Fields
          = do
              P'.wirePutOpt 10 12 x'1
              P'.wirePutOpt 16 13 x'2
              P'.wirePutOpt 24 4 x'3
              P'.wirePutOpt 32 5 x'4
              P'.wirePutOpt 40 3 x'5
              P'.wirePutOpt 50 9 x'6
              P'.wirePutUnknownField x'7
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{escaped_bytes = P'.Just new'Field}) (P'.wireGet 12)
              2 -> P'.fmap (\ new'Field -> old'Self{large_uint32 = P'.Just new'Field}) (P'.wireGet 13)
              3 -> P'.fmap (\ new'Field -> old'Self{large_uint64 = P'.Just new'Field}) (P'.wireGet 4)
              4 -> P'.fmap (\ new'Field -> old'Self{small_int32 = P'.Just new'Field}) (P'.wireGet 5)
              5 -> P'.fmap (\ new'Field -> old'Self{small_int64 = P'.Just new'Field}) (P'.wireGet 3)
              6 -> P'.fmap (\ new'Field -> old'Self{utf8_string = P'.Just new'Field}) (P'.wireGet 9)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestExtremeDefaultValues) TestExtremeDefaultValues where
  getVal m' f' = f' m'
 
instance P'.GPB TestExtremeDefaultValues
 
instance P'.ReflectDescriptor TestExtremeDefaultValues where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestExtremeDefaultValues\"}, descFilePath = [\"UnittestProto\",\"TestExtremeDefaultValues.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestExtremeDefaultValues\", baseName = \"escaped_bytes\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Just (Chunk \"\\NUL\\SOH\\a\\b\\f\\n\\r\\t\\v\\\\'\\\"\\254\" Empty), hsDefault = Just (HsDef'ByteString (Chunk \"\\NUL\\SOH\\a\\b\\f\\n\\r\\t\\v\\\\'\\\"\\254\" Empty))},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestExtremeDefaultValues\", baseName = \"large_uint32\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Just (Chunk \"4294967295\" Empty), hsDefault = Just (HsDef'Integer 4294967295)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestExtremeDefaultValues\", baseName = \"large_uint64\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Just (Chunk \"18446744073709551615\" Empty), hsDefault = Just (HsDef'Integer 18446744073709551615)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestExtremeDefaultValues\", baseName = \"small_int32\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Just (Chunk \"-2147483647\" Empty), hsDefault = Just (HsDef'Integer (-2147483647))},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestExtremeDefaultValues\", baseName = \"small_int64\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Just (Chunk \"-9223372036854775807\" Empty), hsDefault = Just (HsDef'Integer (-9223372036854775807))},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestExtremeDefaultValues\", baseName = \"utf8_string\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Just (Chunk \"\\225\\136\\180\" Empty), hsDefault = Just (HsDef'ByteString (Chunk \"\\225\\136\\180\" Empty))}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"