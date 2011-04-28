import New1.Et.E as Et1.E
import New1.Et.M as M1
import New2u.Et.E as Et2.E
import New2u.Et.M2 as M2
import New2u.Et.M3 as M3

import Data.Sequence as S
import Text.ProtocolBuffers
import Data.ByteString.Lazy as L

m1 :: M1.M 
m1 = defaultValue { M1.es = S.fromList [Et1.E.E1,Et1.E.E2,Et1.E.E3] }

bs1 = messagePut m1

m2 :: Either String (M2.M2,ByteString)
m2 = messageGet bs1

m3 :: Either String (M3.M3,ByteString)
m3 = messageGet bs1
