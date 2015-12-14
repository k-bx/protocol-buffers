import qualified Data.Sequence as Seq
import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.TextMessage
-- 
import Sample.Sample
import Sample.Sample.Sample_message
import Sample.Sample.Sample_message.Test_oneof
import Sample.Sample.Sample_message.Newtest
import Sample.Sample.Sample_message.User

testmsg :: Sample_message
testmsg = Sample_message { key = uFromString "key"
                         , value = Just 30
                         , users = Seq.fromList [ User 30 (Just (uFromString "hello")) Nothing ]
                         , mytest = Nothing
                         , test_oneof = defaultValue
                         , newtest = defaultValue
                         }

main = do
  putStrLn (messagePutText testmsg)
