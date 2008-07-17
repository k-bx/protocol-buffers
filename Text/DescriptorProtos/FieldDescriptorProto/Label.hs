module Text.DescriptorProtos.FieldDescriptorProto.Label (Label(..))
       where
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data Label = LABEL_OPTIONAL
           | LABEL_REQUIRED
           | LABEL_REPEATED
           deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable Label
 
instance P'.Mergeable (P'.Maybe Label) where
        mergeEmpty = P'.Nothing
        mergeAppend = P'.mayMerge
 
instance P'.Bounded Label where
        minBound = LABEL_OPTIONAL
        maxBound = LABEL_REPEATED
 
instance P'.Default Label where
        defaultValue = P'.minBound
 
instance P'.Enum Label where
        fromEnum (LABEL_OPTIONAL) = 1
        fromEnum (LABEL_REQUIRED) = 2
        fromEnum (LABEL_REPEATED) = 3
        toEnum 1 = LABEL_OPTIONAL
        toEnum 2 = LABEL_REQUIRED
        toEnum 3 = LABEL_REPEATED
        succ (LABEL_OPTIONAL) = LABEL_REQUIRED
        succ (LABEL_REQUIRED) = LABEL_REPEATED
        pred (LABEL_REQUIRED) = LABEL_OPTIONAL
        pred (LABEL_REPEATED) = LABEL_REQUIRED
 
instance P'.ReflectEnum Label where
        reflectEnum
          = [(1, "LABEL_OPTIONAL", LABEL_OPTIONAL),
             (2, "LABEL_REQUIRED", LABEL_REQUIRED),
             (3, "LABEL_REPEATED", LABEL_REPEATED)]
        reflectEnumInfo _
          = P'.EnumInfo
              (P'.ProtoName "Text" "DescriptorProtos.FieldDescriptorProto"
                 "Label")
              [(1, "LABEL_OPTIONAL"), (2, "LABEL_REQUIRED"),
               (3, "LABEL_REPEATED")]

{-module Text.DescriptorProtos.FieldDescriptorProto.Label (Label(..))
       where
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data Label = LABEL_OPTIONAL
           | LABEL_REQUIRED
           | LABEL_REPEATED
           deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable Label
 
instance P'.Mergeable (P'.Maybe Label) where
        mergeEmpty = P'.Nothing
        mergeAppend = P'.mayMerge
 
instance P'.Enum Label where
        fromEnum (LABEL_OPTIONAL) = 1
        fromEnum (LABEL_REQUIRED) = 2
        fromEnum (LABEL_REPEATED) = 3
        toEnum 1 = LABEL_OPTIONAL
        toEnum 2 = LABEL_REQUIRED
        toEnum 3 = LABEL_REPEATED
        succ (LABEL_OPTIONAL) = LABEL_REQUIRED
        succ (LABEL_REQUIRED) = LABEL_REPEATED
        pred (LABEL_REQUIRED) = LABEL_OPTIONAL
        pred (LABEL_REPEATED) = LABEL_REQUIRED
 
instance P'.Bounded Label where
        minBound = LABEL_OPTIONAL
        maxBound = LABEL_REPEATED
 
ascList' :: [] (P'.Integer, P'.String, Label)
ascList'
  = [(1, "LABEL_OPTIONAL", LABEL_OPTIONAL),
     (2, "LABEL_REQUIRED", LABEL_REQUIRED),
     (3, "LABEL_REPEATED", LABEL_REPEATED)]

{-
module Text.DescriptorProtos.FieldDescriptorProto.Label
  (Label(..))
 where

import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'

{-
data Label = LABEL_OPTIONAL
           | LABEL_REQUIRED
           | LABEL_REPEATED
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeableEnum ''Label )
-}-}-}