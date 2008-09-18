module UnittestProto.FooRequest (FooRequest(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data FooRequest = FooRequest{}
                deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable FooRequest where
  mergeEmpty = FooRequest
  mergeAppend (FooRequest) (FooRequest) = FooRequest
 
instance P'.Default FooRequest where
  defaultValue = FooRequest
 
instance P'.Wire FooRequest where
  wireSize ft' self'@(FooRequest)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = 0
  wirePut ft' self'@(FooRequest)
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
 
instance P'.MessageAPI msg' (msg' -> FooRequest) FooRequest where
  getVal m' f' = f' m'
 
instance P'.GPB FooRequest
 
instance P'.ReflectDescriptor FooRequest where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"FooRequest\"}, descFilePath = [\"UnittestProto\",\"FooRequest.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList []}"