module UnittestProto.BarResponse (BarResponse(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data BarResponse = BarResponse{}
                 deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable BarResponse where
  mergeEmpty = BarResponse
  mergeAppend (BarResponse) (BarResponse) = BarResponse
 
instance P'.Default BarResponse where
  defaultValue = BarResponse
 
instance P'.Wire BarResponse where
  wireSize ft' self'@(BarResponse)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = 0
  wirePut ft' self'@(BarResponse)
    = case ft' of
        10 -> put'Fields
        11
          -> do
               P'.putSize (P'.wireSize 11 self')
               put'Fields
        _ -> P'.wirePutErr ft' self'
    where
        put'Fields
          = do
              P'.return ()
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessage update'Self
        11 -> P'.getMessage update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> BarResponse) BarResponse where
  getVal m' f' = f' m'
 
instance P'.GPB BarResponse
 
instance P'.ReflectDescriptor BarResponse where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"BarResponse\"}, descFilePath = [\"UnittestProto\",\"BarResponse.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList []}"