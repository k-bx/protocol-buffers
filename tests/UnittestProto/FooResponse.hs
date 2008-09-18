module UnittestProto.FooResponse (FooResponse(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data FooResponse = FooResponse{}
                 deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable FooResponse where
  mergeEmpty = FooResponse
  mergeAppend (FooResponse) (FooResponse) = FooResponse
 
instance P'.Default FooResponse where
  defaultValue = FooResponse
 
instance P'.Wire FooResponse where
  wireSize ft' self'@(FooResponse)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = 0
  wirePut ft' self'@(FooResponse)
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
 
instance P'.MessageAPI msg' (msg' -> FooResponse) FooResponse where
  getVal m' f' = f' m'
 
instance P'.GPB FooResponse
 
instance P'.ReflectDescriptor FooResponse where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"FooResponse\"}, descFilePath = [\"UnittestProto\",\"FooResponse.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList []}"