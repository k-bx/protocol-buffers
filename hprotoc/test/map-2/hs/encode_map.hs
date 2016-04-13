{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
import qualified Data.ByteString.Lazy.Char8       as BL8
import           Text.ProtocolBuffers.Header      (Utf8(..))
import           Text.ProtocolBuffers.WireMessage
import           Some_type.SomeType               (SomeType(..))
import           Mymap.Result                     (Result(..))
import           System.IO                        (stdout)

main :: IO ()
main =
    BL8.hPutStr stdout $ messagePut $
        Result {
            values =
                [ (Utf8 "test"  , SomeType (Utf8 "x") (Utf8 "x")),
                  (Utf8 "test_2", SomeType (Utf8 "y") (Utf8 "y")),
                  (Utf8 "test_2", SomeType (Utf8 "z") (Utf8 "z"))
                ]
        }
