{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
import qualified Data.ByteString.Lazy.Char8       as BL8
import           Text.ProtocolBuffers.Header      (Utf8(..))
import           Text.ProtocolBuffers.WireMessage
import           MapPackage.WithMap               (WithMap(..))
import           MapPackage.Value_type
import           Mymap.Result                     (Result(..))
import           System.IO                        (stdout)
import           GHC.Int

main :: IO ()
main =
    BL8.hPutStr stdout $ messagePut $
        Result {
            fullOfMaps = WithMap {
                another_map_field = [(1::Int32, 2::Int32), (3, 4)],
                map_field =
                    [ (Utf8 "test"  , Value_type (Utf8 "x")),
                      (Utf8 "test_2", Value_type (Utf8 "y"))
                      -- (Utf8 "test_2", Value_type (Utf8 "z"))
                    ]
            }
        }
