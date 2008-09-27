module UnittestProto.TestAllTypes (TestAllTypes(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Com.Google.Protobuf.Test.ImportEnum as Com.Google.Protobuf.Test (ImportEnum)
import qualified Com.Google.Protobuf.Test.ImportMessage as Com.Google.Protobuf.Test (ImportMessage)
import qualified UnittestProto.ForeignEnum as UnittestProto (ForeignEnum)
import qualified UnittestProto.ForeignMessage as UnittestProto (ForeignMessage)
import qualified UnittestProto.TestAllTypes.NestedEnum as UnittestProto.TestAllTypes (NestedEnum)
import qualified UnittestProto.TestAllTypes.NestedMessage as UnittestProto.TestAllTypes (NestedMessage)
import qualified UnittestProto.TestAllTypes.OptionalGroup as UnittestProto.TestAllTypes (OptionalGroup)
import qualified UnittestProto.TestAllTypes.RepeatedGroup as UnittestProto.TestAllTypes (RepeatedGroup)
 
data TestAllTypes = TestAllTypes{optional_int32 :: P'.Maybe P'.Int32, optional_int64 :: P'.Maybe P'.Int64,
                                 optional_uint32 :: P'.Maybe P'.Word32, optional_uint64 :: P'.Maybe P'.Word64,
                                 optional_sint32 :: P'.Maybe P'.Int32, optional_sint64 :: P'.Maybe P'.Int64,
                                 optional_fixed32 :: P'.Maybe P'.Word32, optional_fixed64 :: P'.Maybe P'.Word64,
                                 optional_sfixed32 :: P'.Maybe P'.Int32, optional_sfixed64 :: P'.Maybe P'.Int64,
                                 optional_float :: P'.Maybe P'.Float, optional_double :: P'.Maybe P'.Double,
                                 optional_bool :: P'.Maybe P'.Bool, optional_string :: P'.Maybe P'.Utf8,
                                 optional_bytes :: P'.Maybe P'.ByteString,
                                 optionalGroup :: P'.Maybe UnittestProto.TestAllTypes.OptionalGroup,
                                 optional_nested_message :: P'.Maybe UnittestProto.TestAllTypes.NestedMessage,
                                 optional_foreign_message :: P'.Maybe UnittestProto.ForeignMessage,
                                 optional_import_message :: P'.Maybe Com.Google.Protobuf.Test.ImportMessage,
                                 optional_nested_enum :: P'.Maybe UnittestProto.TestAllTypes.NestedEnum,
                                 optional_foreign_enum :: P'.Maybe UnittestProto.ForeignEnum,
                                 optional_import_enum :: P'.Maybe Com.Google.Protobuf.Test.ImportEnum,
                                 optional_string_piece :: P'.Maybe P'.Utf8, optional_cord :: P'.Maybe P'.Utf8,
                                 repeated_int32 :: P'.Seq P'.Int32, repeated_int64 :: P'.Seq P'.Int64,
                                 repeated_uint32 :: P'.Seq P'.Word32, repeated_uint64 :: P'.Seq P'.Word64,
                                 repeated_sint32 :: P'.Seq P'.Int32, repeated_sint64 :: P'.Seq P'.Int64,
                                 repeated_fixed32 :: P'.Seq P'.Word32, repeated_fixed64 :: P'.Seq P'.Word64,
                                 repeated_sfixed32 :: P'.Seq P'.Int32, repeated_sfixed64 :: P'.Seq P'.Int64,
                                 repeated_float :: P'.Seq P'.Float, repeated_double :: P'.Seq P'.Double,
                                 repeated_bool :: P'.Seq P'.Bool, repeated_string :: P'.Seq P'.Utf8,
                                 repeated_bytes :: P'.Seq P'.ByteString,
                                 repeatedGroup :: P'.Seq UnittestProto.TestAllTypes.RepeatedGroup,
                                 repeated_nested_message :: P'.Seq UnittestProto.TestAllTypes.NestedMessage,
                                 repeated_foreign_message :: P'.Seq UnittestProto.ForeignMessage,
                                 repeated_import_message :: P'.Seq Com.Google.Protobuf.Test.ImportMessage,
                                 repeated_nested_enum :: P'.Seq UnittestProto.TestAllTypes.NestedEnum,
                                 repeated_foreign_enum :: P'.Seq UnittestProto.ForeignEnum,
                                 repeated_import_enum :: P'.Seq Com.Google.Protobuf.Test.ImportEnum,
                                 repeated_string_piece :: P'.Seq P'.Utf8, repeated_cord :: P'.Seq P'.Utf8,
                                 default_int32 :: P'.Maybe P'.Int32, default_int64 :: P'.Maybe P'.Int64,
                                 default_uint32 :: P'.Maybe P'.Word32, default_uint64 :: P'.Maybe P'.Word64,
                                 default_sint32 :: P'.Maybe P'.Int32, default_sint64 :: P'.Maybe P'.Int64,
                                 default_fixed32 :: P'.Maybe P'.Word32, default_fixed64 :: P'.Maybe P'.Word64,
                                 default_sfixed32 :: P'.Maybe P'.Int32, default_sfixed64 :: P'.Maybe P'.Int64,
                                 default_float :: P'.Maybe P'.Float, default_double :: P'.Maybe P'.Double,
                                 default_bool :: P'.Maybe P'.Bool, default_string :: P'.Maybe P'.Utf8,
                                 default_bytes :: P'.Maybe P'.ByteString,
                                 default_nested_enum :: P'.Maybe UnittestProto.TestAllTypes.NestedEnum,
                                 default_foreign_enum :: P'.Maybe UnittestProto.ForeignEnum,
                                 default_import_enum :: P'.Maybe Com.Google.Protobuf.Test.ImportEnum,
                                 default_string_piece :: P'.Maybe P'.Utf8, default_cord :: P'.Maybe P'.Utf8,
                                 unknown'field :: P'.UnknownField}
                  deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage TestAllTypes where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable TestAllTypes where
  mergeEmpty
    = TestAllTypes P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
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
    (TestAllTypes x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23 x'24
       x'25 x'26 x'27 x'28 x'29 x'30 x'31 x'32 x'33 x'34 x'35 x'36 x'37 x'38 x'39 x'40 x'41 x'42 x'43 x'44 x'45 x'46 x'47 x'48 x'49
       x'50 x'51 x'52 x'53 x'54 x'55 x'56 x'57 x'58 x'59 x'60 x'61 x'62 x'63 x'64 x'65 x'66 x'67 x'68 x'69)
    (TestAllTypes y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13 y'14 y'15 y'16 y'17 y'18 y'19 y'20 y'21 y'22 y'23 y'24
       y'25 y'26 y'27 y'28 y'29 y'30 y'31 y'32 y'33 y'34 y'35 y'36 y'37 y'38 y'39 y'40 y'41 y'42 y'43 y'44 y'45 y'46 y'47 y'48 y'49
       y'50 y'51 y'52 y'53 y'54 y'55 y'56 y'57 y'58 y'59 y'60 y'61 y'62 y'63 y'64 y'65 y'66 y'67 y'68 y'69)
    = TestAllTypes (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
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
        (P'.mergeAppend x'35 y'35)
        (P'.mergeAppend x'36 y'36)
        (P'.mergeAppend x'37 y'37)
        (P'.mergeAppend x'38 y'38)
        (P'.mergeAppend x'39 y'39)
        (P'.mergeAppend x'40 y'40)
        (P'.mergeAppend x'41 y'41)
        (P'.mergeAppend x'42 y'42)
        (P'.mergeAppend x'43 y'43)
        (P'.mergeAppend x'44 y'44)
        (P'.mergeAppend x'45 y'45)
        (P'.mergeAppend x'46 y'46)
        (P'.mergeAppend x'47 y'47)
        (P'.mergeAppend x'48 y'48)
        (P'.mergeAppend x'49 y'49)
        (P'.mergeAppend x'50 y'50)
        (P'.mergeAppend x'51 y'51)
        (P'.mergeAppend x'52 y'52)
        (P'.mergeAppend x'53 y'53)
        (P'.mergeAppend x'54 y'54)
        (P'.mergeAppend x'55 y'55)
        (P'.mergeAppend x'56 y'56)
        (P'.mergeAppend x'57 y'57)
        (P'.mergeAppend x'58 y'58)
        (P'.mergeAppend x'59 y'59)
        (P'.mergeAppend x'60 y'60)
        (P'.mergeAppend x'61 y'61)
        (P'.mergeAppend x'62 y'62)
        (P'.mergeAppend x'63 y'63)
        (P'.mergeAppend x'64 y'64)
        (P'.mergeAppend x'65 y'65)
        (P'.mergeAppend x'66 y'66)
        (P'.mergeAppend x'67 y'67)
        (P'.mergeAppend x'68 y'68)
        (P'.mergeAppend x'69 y'69)
 
instance P'.Default TestAllTypes where
  defaultValue
    = TestAllTypes P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
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
        (P'.Just 41)
        (P'.Just 42)
        (P'.Just 43)
        (P'.Just 44)
        (P'.Just (-45))
        (P'.Just 46)
        (P'.Just 47)
        (P'.Just 48)
        (P'.Just 49)
        (P'.Just (-50))
        (P'.Just 51.5)
        (P'.Just 52000.0)
        (P'.Just P'.True)
        (P'.Just (P'.Utf8 (P'.pack "hello")))
        (P'.Just (P'.pack "world"))
        (P'.Just (P'.read "BAR"))
        (P'.Just (P'.read "FOREIGN_BAR"))
        (P'.Just (P'.read "IMPORT_BAR"))
        (P'.Just (P'.Utf8 (P'.pack "abc")))
        (P'.Just (P'.Utf8 (P'.pack "123")))
        P'.defaultValue
 
instance P'.Wire TestAllTypes where
  wireSize ft'
    self'@(TestAllTypes x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23
             x'24 x'25 x'26 x'27 x'28 x'29 x'30 x'31 x'32 x'33 x'34 x'35 x'36 x'37 x'38 x'39 x'40 x'41 x'42 x'43 x'44 x'45 x'46 x'47
             x'48 x'49 x'50 x'51 x'52 x'53 x'54 x'55 x'56 x'57 x'58 x'59 x'60 x'61 x'62 x'63 x'64 x'65 x'66 x'67 x'68 x'69)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
          = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeOpt 1 3 x'2 + P'.wireSizeOpt 1 13 x'3 + P'.wireSizeOpt 1 4 x'4 +
               P'.wireSizeOpt 1 17 x'5
               + P'.wireSizeOpt 1 18 x'6
               + P'.wireSizeOpt 1 7 x'7
               + P'.wireSizeOpt 1 6 x'8
               + P'.wireSizeOpt 1 15 x'9
               + P'.wireSizeOpt 1 16 x'10
               + P'.wireSizeOpt 1 2 x'11
               + P'.wireSizeOpt 1 1 x'12
               + P'.wireSizeOpt 1 8 x'13
               + P'.wireSizeOpt 1 9 x'14
               + P'.wireSizeOpt 1 12 x'15
               + P'.wireSizeOpt 2 10 x'16
               + P'.wireSizeOpt 2 11 x'17
               + P'.wireSizeOpt 2 11 x'18
               + P'.wireSizeOpt 2 11 x'19
               + P'.wireSizeOpt 2 14 x'20
               + P'.wireSizeOpt 2 14 x'21
               + P'.wireSizeOpt 2 14 x'22
               + P'.wireSizeOpt 2 9 x'23
               + P'.wireSizeOpt 2 9 x'24
               + P'.wireSizeRep 2 5 x'25
               + P'.wireSizeRep 2 3 x'26
               + P'.wireSizeRep 2 13 x'27
               + P'.wireSizeRep 2 4 x'28
               + P'.wireSizeRep 2 17 x'29
               + P'.wireSizeRep 2 18 x'30
               + P'.wireSizeRep 2 7 x'31
               + P'.wireSizeRep 2 6 x'32
               + P'.wireSizeRep 2 15 x'33
               + P'.wireSizeRep 2 16 x'34
               + P'.wireSizeRep 2 2 x'35
               + P'.wireSizeRep 2 1 x'36
               + P'.wireSizeRep 2 8 x'37
               + P'.wireSizeRep 2 9 x'38
               + P'.wireSizeRep 2 12 x'39
               + P'.wireSizeRep 2 10 x'40
               + P'.wireSizeRep 2 11 x'41
               + P'.wireSizeRep 2 11 x'42
               + P'.wireSizeRep 2 11 x'43
               + P'.wireSizeRep 2 14 x'44
               + P'.wireSizeRep 2 14 x'45
               + P'.wireSizeRep 2 14 x'46
               + P'.wireSizeRep 2 9 x'47
               + P'.wireSizeRep 2 9 x'48
               + P'.wireSizeOpt 2 5 x'49
               + P'.wireSizeOpt 2 3 x'50
               + P'.wireSizeOpt 2 13 x'51
               + P'.wireSizeOpt 2 4 x'52
               + P'.wireSizeOpt 2 17 x'53
               + P'.wireSizeOpt 2 18 x'54
               + P'.wireSizeOpt 2 7 x'55
               + P'.wireSizeOpt 2 6 x'56
               + P'.wireSizeOpt 2 15 x'57
               + P'.wireSizeOpt 2 16 x'58
               + P'.wireSizeOpt 2 2 x'59
               + P'.wireSizeOpt 2 1 x'60
               + P'.wireSizeOpt 2 8 x'61
               + P'.wireSizeOpt 2 9 x'62
               + P'.wireSizeOpt 2 12 x'63
               + P'.wireSizeOpt 2 14 x'64
               + P'.wireSizeOpt 2 14 x'65
               + P'.wireSizeOpt 2 14 x'66
               + P'.wireSizeOpt 2 9 x'67
               + P'.wireSizeOpt 2 9 x'68
               + P'.wireSizeUnknownField x'69)
  wirePut ft'
    self'@(TestAllTypes x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18 x'19 x'20 x'21 x'22 x'23
             x'24 x'25 x'26 x'27 x'28 x'29 x'30 x'31 x'32 x'33 x'34 x'35 x'36 x'37 x'38 x'39 x'40 x'41 x'42 x'43 x'44 x'45 x'46 x'47
             x'48 x'49 x'50 x'51 x'52 x'53 x'54 x'55 x'56 x'57 x'58 x'59 x'60 x'61 x'62 x'63 x'64 x'65 x'66 x'67 x'68 x'69)
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
              P'.wirePutOpt 16 3 x'2
              P'.wirePutOpt 24 13 x'3
              P'.wirePutOpt 32 4 x'4
              P'.wirePutOpt 45 17 x'5
              P'.wirePutOpt 49 18 x'6
              P'.wirePutOpt 61 7 x'7
              P'.wirePutOpt 65 6 x'8
              P'.wirePutOpt 77 15 x'9
              P'.wirePutOpt 81 16 x'10
              P'.wirePutOpt 93 2 x'11
              P'.wirePutOpt 97 1 x'12
              P'.wirePutOpt 104 8 x'13
              P'.wirePutOpt 114 9 x'14
              P'.wirePutOpt 122 12 x'15
              P'.wirePutOpt 131 10 x'16
              P'.wirePutOpt 146 11 x'17
              P'.wirePutOpt 154 11 x'18
              P'.wirePutOpt 162 11 x'19
              P'.wirePutOpt 168 14 x'20
              P'.wirePutOpt 176 14 x'21
              P'.wirePutOpt 184 14 x'22
              P'.wirePutOpt 194 9 x'23
              P'.wirePutOpt 202 9 x'24
              P'.wirePutRep 248 5 x'25
              P'.wirePutRep 256 3 x'26
              P'.wirePutRep 264 13 x'27
              P'.wirePutRep 272 4 x'28
              P'.wirePutRep 285 17 x'29
              P'.wirePutRep 289 18 x'30
              P'.wirePutRep 301 7 x'31
              P'.wirePutRep 305 6 x'32
              P'.wirePutRep 317 15 x'33
              P'.wirePutRep 321 16 x'34
              P'.wirePutRep 333 2 x'35
              P'.wirePutRep 337 1 x'36
              P'.wirePutRep 344 8 x'37
              P'.wirePutRep 354 9 x'38
              P'.wirePutRep 362 12 x'39
              P'.wirePutRep 371 10 x'40
              P'.wirePutRep 386 11 x'41
              P'.wirePutRep 394 11 x'42
              P'.wirePutRep 402 11 x'43
              P'.wirePutRep 408 14 x'44
              P'.wirePutRep 416 14 x'45
              P'.wirePutRep 424 14 x'46
              P'.wirePutRep 434 9 x'47
              P'.wirePutRep 442 9 x'48
              P'.wirePutOpt 488 5 x'49
              P'.wirePutOpt 496 3 x'50
              P'.wirePutOpt 504 13 x'51
              P'.wirePutOpt 512 4 x'52
              P'.wirePutOpt 525 17 x'53
              P'.wirePutOpt 529 18 x'54
              P'.wirePutOpt 541 7 x'55
              P'.wirePutOpt 545 6 x'56
              P'.wirePutOpt 557 15 x'57
              P'.wirePutOpt 561 16 x'58
              P'.wirePutOpt 573 2 x'59
              P'.wirePutOpt 577 1 x'60
              P'.wirePutOpt 584 8 x'61
              P'.wirePutOpt 594 9 x'62
              P'.wirePutOpt 602 12 x'63
              P'.wirePutOpt 648 14 x'64
              P'.wirePutOpt 656 14 x'65
              P'.wirePutOpt 664 14 x'66
              P'.wirePutOpt 674 9 x'67
              P'.wirePutOpt 682 9 x'68
              P'.wirePutUnknownField x'69
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{optional_int32 = P'.Just new'Field}) (P'.wireGet 5)
              2 -> P'.fmap (\ new'Field -> old'Self{optional_int64 = P'.Just new'Field}) (P'.wireGet 3)
              3 -> P'.fmap (\ new'Field -> old'Self{optional_uint32 = P'.Just new'Field}) (P'.wireGet 13)
              4 -> P'.fmap (\ new'Field -> old'Self{optional_uint64 = P'.Just new'Field}) (P'.wireGet 4)
              5 -> P'.fmap (\ new'Field -> old'Self{optional_sint32 = P'.Just new'Field}) (P'.wireGet 17)
              6 -> P'.fmap (\ new'Field -> old'Self{optional_sint64 = P'.Just new'Field}) (P'.wireGet 18)
              7 -> P'.fmap (\ new'Field -> old'Self{optional_fixed32 = P'.Just new'Field}) (P'.wireGet 7)
              8 -> P'.fmap (\ new'Field -> old'Self{optional_fixed64 = P'.Just new'Field}) (P'.wireGet 6)
              9 -> P'.fmap (\ new'Field -> old'Self{optional_sfixed32 = P'.Just new'Field}) (P'.wireGet 15)
              10 -> P'.fmap (\ new'Field -> old'Self{optional_sfixed64 = P'.Just new'Field}) (P'.wireGet 16)
              11 -> P'.fmap (\ new'Field -> old'Self{optional_float = P'.Just new'Field}) (P'.wireGet 2)
              12 -> P'.fmap (\ new'Field -> old'Self{optional_double = P'.Just new'Field}) (P'.wireGet 1)
              13 -> P'.fmap (\ new'Field -> old'Self{optional_bool = P'.Just new'Field}) (P'.wireGet 8)
              14 -> P'.fmap (\ new'Field -> old'Self{optional_string = P'.Just new'Field}) (P'.wireGet 9)
              15 -> P'.fmap (\ new'Field -> old'Self{optional_bytes = P'.Just new'Field}) (P'.wireGet 12)
              16
                -> P'.fmap (\ new'Field -> old'Self{optionalGroup = P'.mergeAppend (optionalGroup old'Self) (P'.Just new'Field)})
                     (P'.wireGet 10)
              18
                -> P'.fmap
                     (\ new'Field ->
                        old'Self{optional_nested_message = P'.mergeAppend (optional_nested_message old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              19
                -> P'.fmap
                     (\ new'Field ->
                        old'Self{optional_foreign_message = P'.mergeAppend (optional_foreign_message old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              20
                -> P'.fmap
                     (\ new'Field ->
                        old'Self{optional_import_message = P'.mergeAppend (optional_import_message old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              21 -> P'.fmap (\ new'Field -> old'Self{optional_nested_enum = P'.Just new'Field}) (P'.wireGet 14)
              22 -> P'.fmap (\ new'Field -> old'Self{optional_foreign_enum = P'.Just new'Field}) (P'.wireGet 14)
              23 -> P'.fmap (\ new'Field -> old'Self{optional_import_enum = P'.Just new'Field}) (P'.wireGet 14)
              24 -> P'.fmap (\ new'Field -> old'Self{optional_string_piece = P'.Just new'Field}) (P'.wireGet 9)
              25 -> P'.fmap (\ new'Field -> old'Self{optional_cord = P'.Just new'Field}) (P'.wireGet 9)
              31 -> P'.fmap (\ new'Field -> old'Self{repeated_int32 = P'.append (repeated_int32 old'Self) new'Field}) (P'.wireGet 5)
              32 -> P'.fmap (\ new'Field -> old'Self{repeated_int64 = P'.append (repeated_int64 old'Self) new'Field}) (P'.wireGet 3)
              33
                -> P'.fmap (\ new'Field -> old'Self{repeated_uint32 = P'.append (repeated_uint32 old'Self) new'Field})
                     (P'.wireGet 13)
              34
                -> P'.fmap (\ new'Field -> old'Self{repeated_uint64 = P'.append (repeated_uint64 old'Self) new'Field})
                     (P'.wireGet 4)
              35
                -> P'.fmap (\ new'Field -> old'Self{repeated_sint32 = P'.append (repeated_sint32 old'Self) new'Field})
                     (P'.wireGet 17)
              36
                -> P'.fmap (\ new'Field -> old'Self{repeated_sint64 = P'.append (repeated_sint64 old'Self) new'Field})
                     (P'.wireGet 18)
              37
                -> P'.fmap (\ new'Field -> old'Self{repeated_fixed32 = P'.append (repeated_fixed32 old'Self) new'Field})
                     (P'.wireGet 7)
              38
                -> P'.fmap (\ new'Field -> old'Self{repeated_fixed64 = P'.append (repeated_fixed64 old'Self) new'Field})
                     (P'.wireGet 6)
              39
                -> P'.fmap (\ new'Field -> old'Self{repeated_sfixed32 = P'.append (repeated_sfixed32 old'Self) new'Field})
                     (P'.wireGet 15)
              40
                -> P'.fmap (\ new'Field -> old'Self{repeated_sfixed64 = P'.append (repeated_sfixed64 old'Self) new'Field})
                     (P'.wireGet 16)
              41 -> P'.fmap (\ new'Field -> old'Self{repeated_float = P'.append (repeated_float old'Self) new'Field}) (P'.wireGet 2)
              42
                -> P'.fmap (\ new'Field -> old'Self{repeated_double = P'.append (repeated_double old'Self) new'Field})
                     (P'.wireGet 1)
              43 -> P'.fmap (\ new'Field -> old'Self{repeated_bool = P'.append (repeated_bool old'Self) new'Field}) (P'.wireGet 8)
              44
                -> P'.fmap (\ new'Field -> old'Self{repeated_string = P'.append (repeated_string old'Self) new'Field})
                     (P'.wireGet 9)
              45
                -> P'.fmap (\ new'Field -> old'Self{repeated_bytes = P'.append (repeated_bytes old'Self) new'Field}) (P'.wireGet 12)
              46 -> P'.fmap (\ new'Field -> old'Self{repeatedGroup = P'.append (repeatedGroup old'Self) new'Field}) (P'.wireGet 10)
              48
                -> P'.fmap
                     (\ new'Field -> old'Self{repeated_nested_message = P'.append (repeated_nested_message old'Self) new'Field})
                     (P'.wireGet 11)
              49
                -> P'.fmap
                     (\ new'Field -> old'Self{repeated_foreign_message = P'.append (repeated_foreign_message old'Self) new'Field})
                     (P'.wireGet 11)
              50
                -> P'.fmap
                     (\ new'Field -> old'Self{repeated_import_message = P'.append (repeated_import_message old'Self) new'Field})
                     (P'.wireGet 11)
              51
                -> P'.fmap (\ new'Field -> old'Self{repeated_nested_enum = P'.append (repeated_nested_enum old'Self) new'Field})
                     (P'.wireGet 14)
              52
                -> P'.fmap (\ new'Field -> old'Self{repeated_foreign_enum = P'.append (repeated_foreign_enum old'Self) new'Field})
                     (P'.wireGet 14)
              53
                -> P'.fmap (\ new'Field -> old'Self{repeated_import_enum = P'.append (repeated_import_enum old'Self) new'Field})
                     (P'.wireGet 14)
              54
                -> P'.fmap (\ new'Field -> old'Self{repeated_string_piece = P'.append (repeated_string_piece old'Self) new'Field})
                     (P'.wireGet 9)
              55 -> P'.fmap (\ new'Field -> old'Self{repeated_cord = P'.append (repeated_cord old'Self) new'Field}) (P'.wireGet 9)
              61 -> P'.fmap (\ new'Field -> old'Self{default_int32 = P'.Just new'Field}) (P'.wireGet 5)
              62 -> P'.fmap (\ new'Field -> old'Self{default_int64 = P'.Just new'Field}) (P'.wireGet 3)
              63 -> P'.fmap (\ new'Field -> old'Self{default_uint32 = P'.Just new'Field}) (P'.wireGet 13)
              64 -> P'.fmap (\ new'Field -> old'Self{default_uint64 = P'.Just new'Field}) (P'.wireGet 4)
              65 -> P'.fmap (\ new'Field -> old'Self{default_sint32 = P'.Just new'Field}) (P'.wireGet 17)
              66 -> P'.fmap (\ new'Field -> old'Self{default_sint64 = P'.Just new'Field}) (P'.wireGet 18)
              67 -> P'.fmap (\ new'Field -> old'Self{default_fixed32 = P'.Just new'Field}) (P'.wireGet 7)
              68 -> P'.fmap (\ new'Field -> old'Self{default_fixed64 = P'.Just new'Field}) (P'.wireGet 6)
              69 -> P'.fmap (\ new'Field -> old'Self{default_sfixed32 = P'.Just new'Field}) (P'.wireGet 15)
              70 -> P'.fmap (\ new'Field -> old'Self{default_sfixed64 = P'.Just new'Field}) (P'.wireGet 16)
              71 -> P'.fmap (\ new'Field -> old'Self{default_float = P'.Just new'Field}) (P'.wireGet 2)
              72 -> P'.fmap (\ new'Field -> old'Self{default_double = P'.Just new'Field}) (P'.wireGet 1)
              73 -> P'.fmap (\ new'Field -> old'Self{default_bool = P'.Just new'Field}) (P'.wireGet 8)
              74 -> P'.fmap (\ new'Field -> old'Self{default_string = P'.Just new'Field}) (P'.wireGet 9)
              75 -> P'.fmap (\ new'Field -> old'Self{default_bytes = P'.Just new'Field}) (P'.wireGet 12)
              81 -> P'.fmap (\ new'Field -> old'Self{default_nested_enum = P'.Just new'Field}) (P'.wireGet 14)
              82 -> P'.fmap (\ new'Field -> old'Self{default_foreign_enum = P'.Just new'Field}) (P'.wireGet 14)
              83 -> P'.fmap (\ new'Field -> old'Self{default_import_enum = P'.Just new'Field}) (P'.wireGet 14)
              84 -> P'.fmap (\ new'Field -> old'Self{default_string_piece = P'.Just new'Field}) (P'.wireGet 9)
              85 -> P'.fmap (\ new'Field -> old'Self{default_cord = P'.Just new'Field}) (P'.wireGet 9)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestAllTypes) TestAllTypes where
  getVal m' f' = f' m'
 
instance P'.GPB TestAllTypes
 
instance P'.ReflectDescriptor TestAllTypes where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestAllTypes\"}, descFilePath = [\"UnittestProto\",\"TestAllTypes.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_int32\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_int64\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_uint32\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_uint64\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_sint32\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 45}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 17}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_sint64\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 49}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_fixed32\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 61}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 7}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_fixed64\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 65}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 6}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_sfixed32\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 77}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 15}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_sfixed64\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 81}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 16}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_float\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 93}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_double\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 97}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_bool\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 104}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_string\"}, fieldNumber = FieldId {getFieldId = 14}, wireTag = WireTag {getWireTag = 114}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_bytes\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 122}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optionalGroup\"}, fieldNumber = FieldId {getFieldId = 16}, wireTag = WireTag {getWireTag = 131}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 10}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"OptionalGroup\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_nested_message\"}, fieldNumber = FieldId {getFieldId = 18}, wireTag = WireTag {getWireTag = 146}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"NestedMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_foreign_message\"}, fieldNumber = FieldId {getFieldId = 19}, wireTag = WireTag {getWireTag = 154}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_import_message\"}, fieldNumber = FieldId {getFieldId = 20}, wireTag = WireTag {getWireTag = 162}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"Com.Google.Protobuf.Test\", baseName = \"ImportMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_nested_enum\"}, fieldNumber = FieldId {getFieldId = 21}, wireTag = WireTag {getWireTag = 168}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"NestedEnum\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_foreign_enum\"}, fieldNumber = FieldId {getFieldId = 22}, wireTag = WireTag {getWireTag = 176}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignEnum\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_import_enum\"}, fieldNumber = FieldId {getFieldId = 23}, wireTag = WireTag {getWireTag = 184}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"Com.Google.Protobuf.Test\", baseName = \"ImportEnum\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_string_piece\"}, fieldNumber = FieldId {getFieldId = 24}, wireTag = WireTag {getWireTag = 194}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"optional_cord\"}, fieldNumber = FieldId {getFieldId = 25}, wireTag = WireTag {getWireTag = 202}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_int32\"}, fieldNumber = FieldId {getFieldId = 31}, wireTag = WireTag {getWireTag = 248}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_int64\"}, fieldNumber = FieldId {getFieldId = 32}, wireTag = WireTag {getWireTag = 256}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_uint32\"}, fieldNumber = FieldId {getFieldId = 33}, wireTag = WireTag {getWireTag = 264}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_uint64\"}, fieldNumber = FieldId {getFieldId = 34}, wireTag = WireTag {getWireTag = 272}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_sint32\"}, fieldNumber = FieldId {getFieldId = 35}, wireTag = WireTag {getWireTag = 285}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 17}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_sint64\"}, fieldNumber = FieldId {getFieldId = 36}, wireTag = WireTag {getWireTag = 289}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_fixed32\"}, fieldNumber = FieldId {getFieldId = 37}, wireTag = WireTag {getWireTag = 301}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 7}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_fixed64\"}, fieldNumber = FieldId {getFieldId = 38}, wireTag = WireTag {getWireTag = 305}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 6}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_sfixed32\"}, fieldNumber = FieldId {getFieldId = 39}, wireTag = WireTag {getWireTag = 317}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 15}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_sfixed64\"}, fieldNumber = FieldId {getFieldId = 40}, wireTag = WireTag {getWireTag = 321}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 16}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_float\"}, fieldNumber = FieldId {getFieldId = 41}, wireTag = WireTag {getWireTag = 333}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_double\"}, fieldNumber = FieldId {getFieldId = 42}, wireTag = WireTag {getWireTag = 337}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_bool\"}, fieldNumber = FieldId {getFieldId = 43}, wireTag = WireTag {getWireTag = 344}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_string\"}, fieldNumber = FieldId {getFieldId = 44}, wireTag = WireTag {getWireTag = 354}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_bytes\"}, fieldNumber = FieldId {getFieldId = 45}, wireTag = WireTag {getWireTag = 362}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeatedGroup\"}, fieldNumber = FieldId {getFieldId = 46}, wireTag = WireTag {getWireTag = 371}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 10}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"RepeatedGroup\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_nested_message\"}, fieldNumber = FieldId {getFieldId = 48}, wireTag = WireTag {getWireTag = 386}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"NestedMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_foreign_message\"}, fieldNumber = FieldId {getFieldId = 49}, wireTag = WireTag {getWireTag = 394}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_import_message\"}, fieldNumber = FieldId {getFieldId = 50}, wireTag = WireTag {getWireTag = 402}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"Com.Google.Protobuf.Test\", baseName = \"ImportMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_nested_enum\"}, fieldNumber = FieldId {getFieldId = 51}, wireTag = WireTag {getWireTag = 408}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"NestedEnum\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_foreign_enum\"}, fieldNumber = FieldId {getFieldId = 52}, wireTag = WireTag {getWireTag = 416}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignEnum\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_import_enum\"}, fieldNumber = FieldId {getFieldId = 53}, wireTag = WireTag {getWireTag = 424}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"Com.Google.Protobuf.Test\", baseName = \"ImportEnum\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_string_piece\"}, fieldNumber = FieldId {getFieldId = 54}, wireTag = WireTag {getWireTag = 434}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"repeated_cord\"}, fieldNumber = FieldId {getFieldId = 55}, wireTag = WireTag {getWireTag = 442}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_int32\"}, fieldNumber = FieldId {getFieldId = 61}, wireTag = WireTag {getWireTag = 488}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Just (Chunk \"41\" Empty), hsDefault = Just (HsDef'Integer 41)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_int64\"}, fieldNumber = FieldId {getFieldId = 62}, wireTag = WireTag {getWireTag = 496}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Just (Chunk \"42\" Empty), hsDefault = Just (HsDef'Integer 42)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_uint32\"}, fieldNumber = FieldId {getFieldId = 63}, wireTag = WireTag {getWireTag = 504}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 13}, typeName = Nothing, hsRawDefault = Just (Chunk \"43\" Empty), hsDefault = Just (HsDef'Integer 43)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_uint64\"}, fieldNumber = FieldId {getFieldId = 64}, wireTag = WireTag {getWireTag = 512}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Just (Chunk \"44\" Empty), hsDefault = Just (HsDef'Integer 44)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_sint32\"}, fieldNumber = FieldId {getFieldId = 65}, wireTag = WireTag {getWireTag = 525}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 17}, typeName = Nothing, hsRawDefault = Just (Chunk \"-45\" Empty), hsDefault = Just (HsDef'Integer (-45))},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_sint64\"}, fieldNumber = FieldId {getFieldId = 66}, wireTag = WireTag {getWireTag = 529}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 18}, typeName = Nothing, hsRawDefault = Just (Chunk \"46\" Empty), hsDefault = Just (HsDef'Integer 46)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_fixed32\"}, fieldNumber = FieldId {getFieldId = 67}, wireTag = WireTag {getWireTag = 541}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 7}, typeName = Nothing, hsRawDefault = Just (Chunk \"47\" Empty), hsDefault = Just (HsDef'Integer 47)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_fixed64\"}, fieldNumber = FieldId {getFieldId = 68}, wireTag = WireTag {getWireTag = 545}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 6}, typeName = Nothing, hsRawDefault = Just (Chunk \"48\" Empty), hsDefault = Just (HsDef'Integer 48)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_sfixed32\"}, fieldNumber = FieldId {getFieldId = 69}, wireTag = WireTag {getWireTag = 557}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 15}, typeName = Nothing, hsRawDefault = Just (Chunk \"49\" Empty), hsDefault = Just (HsDef'Integer 49)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_sfixed64\"}, fieldNumber = FieldId {getFieldId = 70}, wireTag = WireTag {getWireTag = 561}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 16}, typeName = Nothing, hsRawDefault = Just (Chunk \"-50\" Empty), hsDefault = Just (HsDef'Integer (-50))},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_float\"}, fieldNumber = FieldId {getFieldId = 71}, wireTag = WireTag {getWireTag = 573}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Just (Chunk \"51.5\" Empty), hsDefault = Just (HsDef'Rational (103%2))},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_double\"}, fieldNumber = FieldId {getFieldId = 72}, wireTag = WireTag {getWireTag = 577}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Just (Chunk \"52000.0\" Empty), hsDefault = Just (HsDef'Rational (52000%1))},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_bool\"}, fieldNumber = FieldId {getFieldId = 73}, wireTag = WireTag {getWireTag = 584}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"true\" Empty), hsDefault = Just (HsDef'Bool True)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_string\"}, fieldNumber = FieldId {getFieldId = 74}, wireTag = WireTag {getWireTag = 594}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Just (Chunk \"hello\" Empty), hsDefault = Just (HsDef'ByteString (Chunk \"hello\" Empty))},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_bytes\"}, fieldNumber = FieldId {getFieldId = 75}, wireTag = WireTag {getWireTag = 602}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Just (Chunk \"world\" Empty), hsDefault = Just (HsDef'ByteString (Chunk \"world\" Empty))},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_nested_enum\"}, fieldNumber = FieldId {getFieldId = 81}, wireTag = WireTag {getWireTag = 648}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"NestedEnum\"}), hsRawDefault = Just (Chunk \"BAR\" Empty), hsDefault = Just (HsDef'Enum \"BAR\")},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_foreign_enum\"}, fieldNumber = FieldId {getFieldId = 82}, wireTag = WireTag {getWireTag = 656}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignEnum\"}), hsRawDefault = Just (Chunk \"FOREIGN_BAR\" Empty), hsDefault = Just (HsDef'Enum \"FOREIGN_BAR\")},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_import_enum\"}, fieldNumber = FieldId {getFieldId = 83}, wireTag = WireTag {getWireTag = 664}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"Com.Google.Protobuf.Test\", baseName = \"ImportEnum\"}), hsRawDefault = Just (Chunk \"IMPORT_BAR\" Empty), hsDefault = Just (HsDef'Enum \"IMPORT_BAR\")},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_string_piece\"}, fieldNumber = FieldId {getFieldId = 84}, wireTag = WireTag {getWireTag = 674}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Just (Chunk \"abc\" Empty), hsDefault = Just (HsDef'ByteString (Chunk \"abc\" Empty))},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"default_cord\"}, fieldNumber = FieldId {getFieldId = 85}, wireTag = WireTag {getWireTag = 682}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Just (Chunk \"123\" Empty), hsDefault = Just (HsDef'ByteString (Chunk \"123\" Empty))}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"