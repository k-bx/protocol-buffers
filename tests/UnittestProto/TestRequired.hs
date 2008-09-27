module UnittestProto.TestRequired (TestRequired(..), single, multi) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import {-# SOURCE #-} qualified UnittestProto.TestAllExtensions as UnittestProto (TestAllExtensions)
 
data TestRequired = TestRequired{a :: P'.Int32, dummy2 :: P'.Maybe P'.Int32, b :: P'.Int32, dummy4 :: P'.Maybe P'.Int32,
                                 dummy5 :: P'.Maybe P'.Int32, dummy6 :: P'.Maybe P'.Int32, dummy7 :: P'.Maybe P'.Int32,
                                 dummy8 :: P'.Maybe P'.Int32, dummy9 :: P'.Maybe P'.Int32, dummy10 :: P'.Maybe P'.Int32,
                                 dummy11 :: P'.Maybe P'.Int32, dummy12 :: P'.Maybe P'.Int32, dummy13 :: P'.Maybe P'.Int32,
                                 dummy14 :: P'.Maybe P'.Int32, dummy15 :: P'.Maybe P'.Int32, dummy16 :: P'.Maybe P'.Int32,
                                 dummy17 :: P'.Maybe P'.Int32, dummy18 :: P'.Maybe P'.Int32, dummy19 :: P'.Maybe P'.Int32,
                                 dummy20 :: P'.Maybe P'.Int32, dummy21 :: P'.Maybe P'.Int32, dummy22 :: P'.Maybe P'.Int32,
                                 dummy23 :: P'.Maybe P'.Int32, dummy24 :: P'.Maybe P'.Int32, dummy25 :: P'.Maybe P'.Int32,
                                 dummy26 :: P'.Maybe P'.Int32, dummy27 :: P'.Maybe P'.Int32, dummy28 :: P'.Maybe P'.Int32,
                                 dummy29 :: P'.Maybe P'.Int32, dummy30 :: P'.Maybe P'.Int32, dummy31 :: P'.Maybe P'.Int32,
                                 dummy32 :: P'.Maybe P'.Int32, c :: P'.Int32, unknown'field :: P'.UnknownField}
                  deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
single :: P'.Key P'.Maybe UnittestProto.TestAllExtensions TestRequired
single = P'.Key 1000 11 P'.Nothing
 
multi :: P'.Key P'.Seq UnittestProto.TestAllExtensions TestRequired
multi = P'.Key 1001 11 P'.Nothing
 
instance P'.UnknownMessage TestRequired where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable TestRequired where
  mergeEmpty
    = TestRequired P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
        P'.mergeEmpty
  mergeAppend
    (TestRequired x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23 x'24
       x'25 x'26 x'27 x'28 x'29 x'30 x'31 x'32 x'33 x'34)
    (TestRequired y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13 y'14 y'15 y'16 y'17 y'18 y'19 y'20 y'21 y'22 y'23 y'24
       y'25 y'26 y'27 y'28 y'29 y'30 y'31 y'32 y'33 y'34)
    = TestRequired (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
        (P'.mergeAppend x'5 y'5)
        (P'.mergeAppend x'6 y'6)
        (P'.mergeAppend x'7 y'7)
        (P'.mergeAppend x'8 y'8)
        (P'.mergeAppend x'9 y'9)
        (P'.mergeAppend x'10 y'10)
        (P'.mergeAppend x'11 y'11)
        (P'.mergeAppend x'12 y'12)
        (P'.mergeAppend x'13 y'13)
        (P'.mergeAppend x'14 y'14)
        (P'.mergeAppend x'15 y'15)
        (P'.mergeAppend x'16 y'16)
        (P'.mergeAppend x'17 y'17)
        (P'.mergeAppend x'18 y'18)
        (P'.mergeAppend x'19 y'19)
        (P'.mergeAppend x'20 y'20)
        (P'.mergeAppend x'21 y'21)
        (P'.mergeAppend x'22 y'22)
        (P'.mergeAppend x'23 y'23)
        (P'.mergeAppend x'24 y'24)
        (P'.mergeAppend x'25 y'25)
        (P'.mergeAppend x'26 y'26)
        (P'.mergeAppend x'27 y'27)
        (P'.mergeAppend x'28 y'28)
        (P'.mergeAppend x'29 y'29)
        (P'.mergeAppend x'30 y'30)
        (P'.mergeAppend x'31 y'31)
        (P'.mergeAppend x'32 y'32)
        (P'.mergeAppend x'33 y'33)
        (P'.mergeAppend x'34 y'34)
 
instance P'.Default TestRequired where
  defaultValue
    = TestRequired P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        P'.defaultValue
 
instance P'.Wire TestRequired where
  wireSize ft'
    self'@(TestRequired x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23
             x'24 x'25 x'26 x'27 x'28 x'29 x'30 x'31 x'32 x'33 x'34)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
          = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeOpt 1 5 x'2 + P'.wireSizeReq 1 5 x'3 + P'.wireSizeOpt 1 5 x'4 +
               P'.wireSizeOpt 1 5 x'5
               + P'.wireSizeOpt 1 5 x'6
               + P'.wireSizeOpt 1 5 x'7
               + P'.wireSizeOpt 1 5 x'8
               + P'.wireSizeOpt 1 5 x'9
               + P'.wireSizeOpt 1 5 x'10
               + P'.wireSizeOpt 1 5 x'11
               + P'.wireSizeOpt 1 5 x'12
               + P'.wireSizeOpt 1 5 x'13
               + P'.wireSizeOpt 1 5 x'14
               + P'.wireSizeOpt 1 5 x'15
               + P'.wireSizeOpt 2 5 x'16
               + P'.wireSizeOpt 2 5 x'17
               + P'.wireSizeOpt 2 5 x'18
               + P'.wireSizeOpt 2 5 x'19
               + P'.wireSizeOpt 2 5 x'20
               + P'.wireSizeOpt 2 5 x'21
               + P'.wireSizeOpt 2 5 x'22
               + P'.wireSizeOpt 2 5 x'23
               + P'.wireSizeOpt 2 5 x'24
               + P'.wireSizeOpt 2 5 x'25
               + P'.wireSizeOpt 2 5 x'26
               + P'.wireSizeOpt 2 5 x'27
               + P'.wireSizeOpt 2 5 x'28
               + P'.wireSizeOpt 2 5 x'29
               + P'.wireSizeOpt 2 5 x'30
               + P'.wireSizeOpt 2 5 x'31
               + P'.wireSizeOpt 2 5 x'32
               + P'.wireSizeReq 2 5 x'33
               + P'.wireSizeUnknownField x'34)
  wirePut ft'
    self'@(TestRequired x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23
             x'24 x'25 x'26 x'27 x'28 x'29 x'30 x'31 x'32 x'33 x'34)
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
              P'.wirePutReq 8 5 x'1
              P'.wirePutOpt 16 5 x'2
              P'.wirePutReq 24 5 x'3
              P'.wirePutOpt 32 5 x'4
              P'.wirePutOpt 40 5 x'5
              P'.wirePutOpt 48 5 x'6
              P'.wirePutOpt 56 5 x'7
              P'.wirePutOpt 64 5 x'8
              P'.wirePutOpt 72 5 x'9
              P'.wirePutOpt 80 5 x'10
              P'.wirePutOpt 88 5 x'11
              P'.wirePutOpt 96 5 x'12
              P'.wirePutOpt 104 5 x'13
              P'.wirePutOpt 112 5 x'14
              P'.wirePutOpt 120 5 x'15
              P'.wirePutOpt 128 5 x'16
              P'.wirePutOpt 136 5 x'17
              P'.wirePutOpt 144 5 x'18
              P'.wirePutOpt 152 5 x'19
              P'.wirePutOpt 160 5 x'20
              P'.wirePutOpt 168 5 x'21
              P'.wirePutOpt 176 5 x'22
              P'.wirePutOpt 184 5 x'23
              P'.wirePutOpt 192 5 x'24
              P'.wirePutOpt 200 5 x'25
              P'.wirePutOpt 208 5 x'26
              P'.wirePutOpt 216 5 x'27
              P'.wirePutOpt 224 5 x'28
              P'.wirePutOpt 232 5 x'29
              P'.wirePutOpt 240 5 x'30
              P'.wirePutOpt 248 5 x'31
              P'.wirePutOpt 256 5 x'32
              P'.wirePutReq 264 5 x'33
              P'.wirePutUnknownField x'34
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{a = new'Field}) (P'.wireGet 5)
              2 -> P'.fmap (\ new'Field -> old'Self{dummy2 = P'.Just new'Field}) (P'.wireGet 5)
              3 -> P'.fmap (\ new'Field -> old'Self{b = new'Field}) (P'.wireGet 5)
              4 -> P'.fmap (\ new'Field -> old'Self{dummy4 = P'.Just new'Field}) (P'.wireGet 5)
              5 -> P'.fmap (\ new'Field -> old'Self{dummy5 = P'.Just new'Field}) (P'.wireGet 5)
              6 -> P'.fmap (\ new'Field -> old'Self{dummy6 = P'.Just new'Field}) (P'.wireGet 5)
              7 -> P'.fmap (\ new'Field -> old'Self{dummy7 = P'.Just new'Field}) (P'.wireGet 5)
              8 -> P'.fmap (\ new'Field -> old'Self{dummy8 = P'.Just new'Field}) (P'.wireGet 5)
              9 -> P'.fmap (\ new'Field -> old'Self{dummy9 = P'.Just new'Field}) (P'.wireGet 5)
              10 -> P'.fmap (\ new'Field -> old'Self{dummy10 = P'.Just new'Field}) (P'.wireGet 5)
              11 -> P'.fmap (\ new'Field -> old'Self{dummy11 = P'.Just new'Field}) (P'.wireGet 5)
              12 -> P'.fmap (\ new'Field -> old'Self{dummy12 = P'.Just new'Field}) (P'.wireGet 5)
              13 -> P'.fmap (\ new'Field -> old'Self{dummy13 = P'.Just new'Field}) (P'.wireGet 5)
              14 -> P'.fmap (\ new'Field -> old'Self{dummy14 = P'.Just new'Field}) (P'.wireGet 5)
              15 -> P'.fmap (\ new'Field -> old'Self{dummy15 = P'.Just new'Field}) (P'.wireGet 5)
              16 -> P'.fmap (\ new'Field -> old'Self{dummy16 = P'.Just new'Field}) (P'.wireGet 5)
              17 -> P'.fmap (\ new'Field -> old'Self{dummy17 = P'.Just new'Field}) (P'.wireGet 5)
              18 -> P'.fmap (\ new'Field -> old'Self{dummy18 = P'.Just new'Field}) (P'.wireGet 5)
              19 -> P'.fmap (\ new'Field -> old'Self{dummy19 = P'.Just new'Field}) (P'.wireGet 5)
              20 -> P'.fmap (\ new'Field -> old'Self{dummy20 = P'.Just new'Field}) (P'.wireGet 5)
              21 -> P'.fmap (\ new'Field -> old'Self{dummy21 = P'.Just new'Field}) (P'.wireGet 5)
              22 -> P'.fmap (\ new'Field -> old'Self{dummy22 = P'.Just new'Field}) (P'.wireGet 5)
              23 -> P'.fmap (\ new'Field -> old'Self{dummy23 = P'.Just new'Field}) (P'.wireGet 5)
              24 -> P'.fmap (\ new'Field -> old'Self{dummy24 = P'.Just new'Field}) (P'.wireGet 5)
              25 -> P'.fmap (\ new'Field -> old'Self{dummy25 = P'.Just new'Field}) (P'.wireGet 5)
              26 -> P'.fmap (\ new'Field -> old'Self{dummy26 = P'.Just new'Field}) (P'.wireGet 5)
              27 -> P'.fmap (\ new'Field -> old'Self{dummy27 = P'.Just new'Field}) (P'.wireGet 5)
              28 -> P'.fmap (\ new'Field -> old'Self{dummy28 = P'.Just new'Field}) (P'.wireGet 5)
              29 -> P'.fmap (\ new'Field -> old'Self{dummy29 = P'.Just new'Field}) (P'.wireGet 5)
              30 -> P'.fmap (\ new'Field -> old'Self{dummy30 = P'.Just new'Field}) (P'.wireGet 5)
              31 -> P'.fmap (\ new'Field -> old'Self{dummy31 = P'.Just new'Field}) (P'.wireGet 5)
              32 -> P'.fmap (\ new'Field -> old'Self{dummy32 = P'.Just new'Field}) (P'.wireGet 5)
              33 -> P'.fmap (\ new'Field -> old'Self{c = new'Field}) (P'.wireGet 5)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestRequired) TestRequired where
  getVal m' f' = f' m'
 
instance P'.GPB TestRequired
 
instance P'.ReflectDescriptor TestRequired where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestRequired\"}, descFilePath = [\"UnittestProto\",\"TestRequired.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"a\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = True, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy2\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"b\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, wireTagLength = 1, isRequired = True, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy4\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy5\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy6\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy7\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 56}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy8\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 64}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy9\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy10\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy11\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 88}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy12\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 96}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy13\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 104}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy14\"}, fieldNumber = FieldId {getFieldId = 14}, wireTag = WireTag {getWireTag = 112}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy15\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 120}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy16\"}, fieldNumber = FieldId {getFieldId = 16}, wireTag = WireTag {getWireTag = 128}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy17\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 136}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy18\"}, fieldNumber = FieldId {getFieldId = 18}, wireTag = WireTag {getWireTag = 144}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy19\"}, fieldNumber = FieldId {getFieldId = 19}, wireTag = WireTag {getWireTag = 152}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy20\"}, fieldNumber = FieldId {getFieldId = 20}, wireTag = WireTag {getWireTag = 160}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy21\"}, fieldNumber = FieldId {getFieldId = 21}, wireTag = WireTag {getWireTag = 168}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy22\"}, fieldNumber = FieldId {getFieldId = 22}, wireTag = WireTag {getWireTag = 176}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy23\"}, fieldNumber = FieldId {getFieldId = 23}, wireTag = WireTag {getWireTag = 184}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy24\"}, fieldNumber = FieldId {getFieldId = 24}, wireTag = WireTag {getWireTag = 192}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy25\"}, fieldNumber = FieldId {getFieldId = 25}, wireTag = WireTag {getWireTag = 200}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy26\"}, fieldNumber = FieldId {getFieldId = 26}, wireTag = WireTag {getWireTag = 208}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy27\"}, fieldNumber = FieldId {getFieldId = 27}, wireTag = WireTag {getWireTag = 216}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy28\"}, fieldNumber = FieldId {getFieldId = 28}, wireTag = WireTag {getWireTag = 224}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy29\"}, fieldNumber = FieldId {getFieldId = 29}, wireTag = WireTag {getWireTag = 232}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy30\"}, fieldNumber = FieldId {getFieldId = 30}, wireTag = WireTag {getWireTag = 240}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy31\"}, fieldNumber = FieldId {getFieldId = 31}, wireTag = WireTag {getWireTag = 248}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"dummy32\"}, fieldNumber = FieldId {getFieldId = 32}, wireTag = WireTag {getWireTag = 256}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"c\"}, fieldNumber = FieldId {getFieldId = 33}, wireTag = WireTag {getWireTag = 264}, wireTagLength = 2, isRequired = True, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [(ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestAllExtensions\"},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"single\"}, fieldNumber = FieldId {getFieldId = 1000}, wireTag = WireTag {getWireTag = 8002}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestRequired\"}), hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestAllExtensions\"},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequired\", baseName = \"multi\"}, fieldNumber = FieldId {getFieldId = 1001}, wireTag = WireTag {getWireTag = 8010}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestRequired\"}), hsRawDefault = Nothing, hsDefault = Nothing})], extRanges = [], knownKeys = fromList [], storeUnknown = True}"