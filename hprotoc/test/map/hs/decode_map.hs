{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Lazy.Char8       as BL8
import           Text.ProtocolBuffers.WireMessage
import           Mymap.WithMap (WithMap)
import           System.IO (stdin)

main :: IO ()
main = do
    msg :: Either String (WithMap, BL8.ByteString)
        <- messageGet <$> BL8.hGetContents stdin
    case msg of
        Left err -> fail err
        Right (x, _)  -> print x
