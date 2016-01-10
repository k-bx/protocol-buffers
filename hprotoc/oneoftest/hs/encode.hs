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

grynffindor = Dormitory { School.Dormitory.name = uFromString "Grynffindor"
                        , members = Seq.fromList [ dumbledore, harrypotter ] }

dumbledore  = Member { School.Member.id = 1
                     , School.Member.name = uFromString "Albus Dumbledore"
                     , property = Just . Prop_faculty $
                                    Faculty { subject = uFromString "allmighty"
                                            , School.Member.Faculty.title =
                                                Just (uFromString "headmaster")
                                            , duty = Seq.empty }
                     }

harrypotter = Member { School.Member.id = 2
                     , School.Member.name = uFromString "Harry Potter"
                     , property = Just . Prop_student $
                                    Student { grade = 5
                                            , specialty = Just (uFromString "defense of dark arts") }
                     }
              
main = do
  args <- getArgs
  let n = length args
  if n /= 1
    then (putStrLn "Usage: encode FILE")
    else do
      let fn = args !! 0
          encoded = messagePut grynffindor
      LB.writeFile fn encoded

 
