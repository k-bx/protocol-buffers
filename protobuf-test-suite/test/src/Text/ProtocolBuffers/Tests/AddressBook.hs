module Text.ProtocolBuffers.Tests.AddressBook
  ( addressBookTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.ByteString.Lazy.Char8 as LB

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.TextMessage
import Text.ProtocolBuffers.WireMessage

import qualified HSCodeGen.AddressBookProtos.AddressBook        as AddressBook'
import qualified HSCodeGen.AddressBookProtos.Person             as Person'
import qualified HSCodeGen.AddressBookProtos.Person.PhoneNumber as PhoneNumber'
import HSCodeGen.AddressBookProtos.AddressBook        (AddressBook(..))
import HSCodeGen.AddressBookProtos.Person             (Person(..))
import HSCodeGen.AddressBookProtos.Person.PhoneNumber (PhoneNumber(..))
import HSCodeGen.AddressBookProtos.Person.PhoneType   (PhoneType(..))

addressBookTests :: TestTree
addressBookTests = testGroup "Address book tests"
  [ testCase "Address book text-encoded then decoded should be an identity" $
      roundTripTextEncodeDecode @? "text-encoded then decoded was not an identity"
  ]

roundTripTextEncodeDecode :: Bool
roundTripTextEncodeDecode =
  let encoded = messagePutText addressBook
      decoded = case messageGetText $ LB.pack encoded of
                  Left _ -> False
                  Right result -> result == addressBook
  in decoded

addressBook :: AddressBook
addressBook =
  AddressBook {
    AddressBook'.person = Seq.fromList
      [ mkPerson "Alice" 1 (Just "alice@example.com") $ Seq.singleton ("123-456-7890", HOME)
      , mkPerson "Bob" 2 Nothing $ Seq.fromList [("1-800-123-4567", MOBILE), ("604-291-1234", WORK)]
      ]
  , AddressBook'.unknown'field = defaultValue
  }

mkPerson :: String -> Int -> Maybe String -> Seq (String, PhoneType) -> Person
mkPerson name id' email phoneNumbers =
  Person {
    Person'.name = uFromString name
  , Person'.id = fromIntegral id'
  , Person'.email = uFromString <$> email
  , Person'.phone = mkPhoneNumbers phoneNumbers
  , Person'.unknown'field = defaultValue
  }

mkPhoneNumbers :: Seq (String, PhoneType) -> Seq PhoneNumber
mkPhoneNumbers = fmap mkPhoneNumbers' where
  mkPhoneNumbers' (num, t) =
    PhoneNumber {
      PhoneNumber'.number = uFromString num
    , PhoneNumber'.type' = Just t
    , PhoneNumber'.unknown'field = defaultValue
    }

