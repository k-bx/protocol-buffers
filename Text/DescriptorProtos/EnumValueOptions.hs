module Text.DescriptorProtos.EnumValueOptions (EnumValueOptions(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data EnumValueOptions = EnumValueOptions{}
                      deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable EnumValueOptions where
  mergeEmpty = EnumValueOptions
  mergeAppend (EnumValueOptions) (EnumValueOptions) = EnumValueOptions
 
instance P'.Default EnumValueOptions where
  defaultValue = EnumValueOptions
 
instance P'.Wire EnumValueOptions where
  wireSize ft' (EnumValueOptions)
    = case ft' of
        10 -> calc'Size
        11 -> calc'Size
    where
        calc'Size = 0
  wirePut ft' self'@(EnumValueOptions)
    = case ft' of
        10 -> put'Fields
        11
          -> do
               P'.putSize (P'.wireSize 11 self')
               put'Fields
    where
        put'Fields
          = do
              P'.return ()
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessage update'Self
        11 -> P'.getMessage update'Self
    where
        update'Self field'Number old'Self
          = case field'Number of
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> EnumValueOptions) EnumValueOptions where
  getVal m' f' = f' m'
 
instance P'.GPB EnumValueOptions
 
instance P'.ReflectDescriptor EnumValueOptions where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumValueOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"EnumValueOptions.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList []}"