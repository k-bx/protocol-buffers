module UnittestProto.TestCamelCaseFieldNames (TestCamelCaseFieldNames(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified UnittestProto.ForeignEnum as UnittestProto (ForeignEnum)
import qualified UnittestProto.ForeignMessage as UnittestProto (ForeignMessage)
 
data TestCamelCaseFieldNames = TestCamelCaseFieldNames{primitiveField :: P'.Maybe P'.Int32, stringField :: P'.Maybe P'.Utf8,
                                                       enumField :: P'.Maybe UnittestProto.ForeignEnum,
                                                       messageField :: P'.Maybe UnittestProto.ForeignMessage,
                                                       stringPieceField :: P'.Maybe P'.Utf8, cordField :: P'.Maybe P'.Utf8,
                                                       repeatedPrimitiveField :: P'.Seq P'.Int32,
                                                       repeatedStringField :: P'.Seq P'.Utf8,
                                                       repeatedEnumField :: P'.Seq UnittestProto.ForeignEnum,
                                                       repeatedMessageField :: P'.Seq UnittestProto.ForeignMessage,
                                                       repeatedStringPieceField :: P'.Seq P'.Utf8,
                                                       repeatedCordField :: P'.Seq P'.Utf8}
                             deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable TestCamelCaseFieldNames where
  mergeEmpty
    = TestCamelCaseFieldNames P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
  mergeAppend (TestCamelCaseFieldNames x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12)
    (TestCamelCaseFieldNames y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12)
    = TestCamelCaseFieldNames (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
        (P'.mergeAppend x'5 y'5)
        (P'.mergeAppend x'6 y'6)
        (P'.mergeAppend x'7 y'7)
        (P'.mergeAppend x'8 y'8)
        (P'.mergeAppend x'9 y'9)
        (P'.mergeAppend x'10 y'10)
        (P'.mergeAppend x'11 y'11)
        (P'.mergeAppend x'12 y'12)
 
instance P'.Default TestCamelCaseFieldNames where
  defaultValue
    = TestCamelCaseFieldNames P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
 
instance P'.Wire TestCamelCaseFieldNames where
  wireSize ft' self'@(TestCamelCaseFieldNames x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
          = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 14 x'3 + P'.wireSizeOpt 1 11 x'4 +
               P'.wireSizeOpt 1 9 x'5
               + P'.wireSizeOpt 1 9 x'6
               + P'.wireSizeRep 1 5 x'7
               + P'.wireSizeRep 1 9 x'8
               + P'.wireSizeRep 1 14 x'9
               + P'.wireSizeRep 1 11 x'10
               + P'.wireSizeRep 1 9 x'11
               + P'.wireSizeRep 1 9 x'12)
  wirePut ft' self'@(TestCamelCaseFieldNames x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12)
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
              P'.wirePutOpt 8 5 x'1
              P'.wirePutOpt 18 9 x'2
              P'.wirePutOpt 24 14 x'3
              P'.wirePutOpt 34 11 x'4
              P'.wirePutOpt 42 9 x'5
              P'.wirePutOpt 50 9 x'6
              P'.wirePutRep 56 5 x'7
              P'.wirePutRep 66 9 x'8
              P'.wirePutRep 72 14 x'9
              P'.wirePutRep 82 11 x'10
              P'.wirePutRep 90 9 x'11
              P'.wirePutRep 98 9 x'12
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessage update'Self
        11 -> P'.getMessage update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{primitiveField = P'.Just new'Field}) (P'.wireGet 5)
              2 -> P'.fmap (\ new'Field -> old'Self{stringField = P'.Just new'Field}) (P'.wireGet 9)
              3 -> P'.fmap (\ new'Field -> old'Self{enumField = P'.Just new'Field}) (P'.wireGet 14)
              4 -> P'.fmap (\ new'Field -> old'Self{messageField = P'.mergeAppend (messageField old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              5 -> P'.fmap (\ new'Field -> old'Self{stringPieceField = P'.Just new'Field}) (P'.wireGet 9)
              6 -> P'.fmap (\ new'Field -> old'Self{cordField = P'.Just new'Field}) (P'.wireGet 9)
              7 -> P'.fmap (\ new'Field -> old'Self{repeatedPrimitiveField = P'.append (repeatedPrimitiveField old'Self) new'Field})
                     (P'.wireGet 5)
              8 -> P'.fmap (\ new'Field -> old'Self{repeatedStringField = P'.append (repeatedStringField old'Self) new'Field})
                     (P'.wireGet 9)
              9 -> P'.fmap (\ new'Field -> old'Self{repeatedEnumField = P'.append (repeatedEnumField old'Self) new'Field})
                     (P'.wireGet 14)
              10
                -> P'.fmap (\ new'Field -> old'Self{repeatedMessageField = P'.append (repeatedMessageField old'Self) new'Field})
                     (P'.wireGet 11)
              11
                -> P'.fmap
                     (\ new'Field -> old'Self{repeatedStringPieceField = P'.append (repeatedStringPieceField old'Self) new'Field})
                     (P'.wireGet 9)
              12
                -> P'.fmap (\ new'Field -> old'Self{repeatedCordField = P'.append (repeatedCordField old'Self) new'Field})
                     (P'.wireGet 9)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestCamelCaseFieldNames) TestCamelCaseFieldNames where
  getVal m' f' = f' m'
 
instance P'.GPB TestCamelCaseFieldNames
 
instance P'.ReflectDescriptor TestCamelCaseFieldNames where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestCamelCaseFieldNames\"}, descFilePath = [\"UnittestProto\",\"TestCamelCaseFieldNames.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"primitiveField\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"stringField\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"enumField\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignEnum\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"messageField\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"stringPieceField\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"cordField\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"repeatedPrimitiveField\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 56}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"repeatedStringField\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"repeatedEnumField\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignEnum\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"repeatedMessageField\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"repeatedStringPieceField\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestCamelCaseFieldNames\", baseName = \"repeatedCordField\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 98}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList []}"