module Text.DescriptorProtos.FieldOptions.CType (CType(..)) where
import Prelude ((+), (++))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data CType = CORD
           | STRING_PIECE
           deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable CType
 
instance P'.Bounded CType where
        minBound = CORD
        maxBound = STRING_PIECE
 
instance P'.Default CType where
        defaultValue = CORD
 
instance P'.Enum CType where
        fromEnum (CORD) = 1
        fromEnum (STRING_PIECE) = 2
        toEnum 1 = CORD
        toEnum 2 = STRING_PIECE
        succ (CORD) = STRING_PIECE
        pred (STRING_PIECE) = CORD
 
instance P'.Wire CType where
        wireSize 14 enum = P'.wireSize 14 (P'.fromEnum enum)
        wirePut 14 enum = P'.wirePut 14 (P'.fromEnum enum)
        wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
 
instance P'.ReflectEnum CType where
        reflectEnum
          = [(1, "CORD", CORD), (2, "STRING_PIECE", STRING_PIECE)]
        reflectEnumInfo _
          = P'.EnumInfo
              (P'.ProtoName "Text" "DescriptorProtos.FieldOptions" "CType")
              [(1, "CORD"), (2, "STRING_PIECE")]