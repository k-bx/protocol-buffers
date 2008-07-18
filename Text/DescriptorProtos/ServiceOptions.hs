module Text.DescriptorProtos.ServiceOptions
  (ServiceOptions(..))
 where

import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'

data ServiceOptions = ServiceOptions{}
                    deriving (P'.Show, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable ServiceOptions where
        mergeEmpty = ServiceOptions
        mergeAppend (ServiceOptions) (ServiceOptions) = ServiceOptions
 
instance P'.Default ServiceOptions

{-
data ServiceOptions = ServiceOptions{field1 ::
                                     P'.Maybe P'.ByteString,
                                     field2 :: P'.Maybe ServiceOptions}
                    deriving (P'.Show, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable ServiceOptions where
        mergeEmpty = ServiceOptions P'.mergeEmpty P'.mergeEmpty
        mergeAppend (ServiceOptions x'1 x'2) (ServiceOptions y'1 y'2)
          = ServiceOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Mergeable (P'.Maybe ServiceOptions) where
        mergeEmpty = P'.Nothing
        mergeAppend = P'.mayMerge
 
instance P'.Default ServiceOptions
-}
{-
data ServiceOptions = ServiceOptions
  deriving (P'.Show,P'.Eq,P'.Ord,P'.Typeable)

instance P'.Mergeable ServiceOptions where
        mergeEmpty = ServiceOptions
        mergeAppend = P'.mergeAppend
 
instance P'.Mergeable (P'.Maybe ServiceOptions) where
        mergeEmpty = P'.Nothing
        mergeAppend = P'.mayMerge

instance P'.Default ServiceOptions
-}