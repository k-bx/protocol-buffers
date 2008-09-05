module $PARENT.$NAME ($NAME) where

import qualified Prelude as P'(Show,Eq,Ord,Maybe,Double,Float)
import qualified Text.ProtocolBuffers.Header as P'(Typeable,Mergeable,Default,Wire,MessageAPI,GPB,ReflectDescriptor
                                                  ,Seq,Utf8,ByteString,Int32,Int64,Word32,Word64,ExtendMessage)

data $NAME

instance P'.Show $NAME
instance P'.Eq $NAME
instance P'.Ord $NAME
instance P'.Typeable $NAME
instance P'.Mergeable $NAME
instance P'.Default $NAME
instance P'.Wire $NAME
instance P'.MessageAPI msg' (msg' -> $NAME) $NAME
instance P'.GPB $NAME
instance P'.ExtendMessage $NAME
instance P'.ReflectDescriptor $NAME
