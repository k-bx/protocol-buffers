{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
import qualified Data.ByteString.Lazy.Char8       as BL8
import           Text.ProtocolBuffers.Header      (Utf8(..))
import           Text.ProtocolBuffers.WireMessage
import           Mymap.Value_type
import           Mymap.WithMap                    (WithMap(..))
import           System.IO                        (stdout)

main :: IO ()
main =
    BL8.hPutStr stdout $ messagePut $
        WithMap {
            map_field =
                [ (Utf8 "test"  , Value_type (Utf8 "x")),
                  (Utf8 "test_2", Value_type (Utf8 "y")),
                  (Utf8 "test_2", Value_type (Utf8 "z"))
                ],
            another_map_field =
                [ (1, 1),
                  (2, 2),
                  (3, 3)
                ]
        }
