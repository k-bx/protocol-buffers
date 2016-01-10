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
import School.Dormitory
import School.Member
import School.Member.Property
import School.Member.Faculty
import School.Member.Student
import School.Member.Admin
--

main = do
  args <- getArgs
  let n = length args
  if n /= 1
    then (putStrLn "Usage: decode FILE")
    else do
      let fn = args !! 0
      lbstr <- LB.readFile fn
      let m = messageGet lbstr :: Either String (Dormitory,LB.ByteString)
      print m

 
