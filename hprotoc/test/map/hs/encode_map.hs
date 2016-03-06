{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Sequence as Seq
import System.Environment
import System.IO
import qualified Text.ProtocolBuffers.Header as P'
--
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.TextMessage
import Text.ProtocolBuffers.WireMessage
--
import Mymap
import Mymap.Value_type
import Mymap.WithMap (WithMap(..))
import Mymap.WithMap.Map_field_Entry (Map_field_Entry(..))
import qualified Mymap.WithMap.Another_map_field_Entry as X (Another_map_field_Entry(..))
--

main :: IO ()
main = LB.writeFile "mymap.output" $ messagePut $
    WithMap {
        map_field =
            Seq.fromList [
                Map_field_Entry {key = P'.Utf8 "test"  , value = Value_type $ P'.Utf8 "x"},
                Map_field_Entry {key = P'.Utf8 "test_2", value = Value_type $ P'.Utf8 "y"},
                Map_field_Entry {key = P'.Utf8 "test_2", value = Value_type $ P'.Utf8 "z"}
            ],
        another_map_field = Seq.fromList [
                X.Another_map_field_Entry {X.key=1, X.value=1}
            ]
    }
