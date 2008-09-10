module Text.DescriptorProtos.EnumOptions (EnumOptions(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data EnumOptions = EnumOptions{}
                 deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable EnumOptions where
  mergeEmpty = EnumOptions
  mergeAppend (EnumOptions) (EnumOptions) = EnumOptions
 
instance P'.Default EnumOptions where
  defaultValue = EnumOptions
 
instance P'.Wire EnumOptions where
  wireSize ft' (EnumOptions)
    = case ft' of
        10 -> calc'Size
        11 -> calc'Size
    where
        calc'Size = 0
  wirePut ft' self'@(EnumOptions)
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
 
instance P'.MessageAPI msg' (msg' -> EnumOptions) EnumOptions where
  getVal m' f' = f' m'
 
instance P'.GPB EnumOptions
 
instance P'.ReflectDescriptor EnumOptions where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"EnumOptions.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList []}"