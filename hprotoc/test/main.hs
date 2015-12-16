import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Sequence as Seq
import System.Environment
import System.IO
--
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.TextMessage
import Text.ProtocolBuffers.WireMessage
-- 
import Sample.Sample
import Sample.Sample.Sample_message
import Sample.Sample.Sample_message.Test_oneof
import Sample.Sample.Sample_message.Newtest
import Sample.Sample.Sample_message.User
--

testmsg :: Sample_message
testmsg = Sample_message { key = uFromString "key"
                         , value = Just 30
                         , users = Seq.fromList [ User 30 (Just (uFromString "hello")) Nothing ]
                         , mytest = Nothing
                         , test_oneof = Just (Age 38)
                         , newtest = defaultValue
                         }


main' = do
  let encoded = messagePut testmsg
      decoded = messageGet encoded :: Either String (Sample_message,LB.ByteString)
  -- putStrLn encoded
  print decoded

  LB.writeFile "binarytest.dat" encoded


main = do
  let fn = args !! 0
  lbstr <- LB.readFile fn
  let m = messageGet lbstr :: Either String (Sample_message,LB.ByteString)
  print m

 
