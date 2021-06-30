{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Text.ProtocolBuffers.Test.Tests.AddressBook
  ( addressBookTests
  , addressBookQuickChecks
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck ()
import Data.Proxy

import qualified Data.Aeson as J
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Applicative (liftA)

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
import Text.ProtocolBuffers.Test.QuickCheck (quickCheckTests)

addressBookTests :: TestTree
addressBookTests = testGroup "Address book tests"
  [ testCase "Text-encode then decode identity" $
      roundTripTextEncodeDecode addressBook1 @?= Just addressBook1
  , testCase "Text-decoded empty optional value should be decoded to its default" $
      roundTripTextEncodeDecode addressBook2 @?= (Just . mapDefaultPhoneType $ addressBook2)
  -- , testCase "Text-encode then decode identity 2" $
  --     roundTripTextEncodeDecode addressBook3 @?= Just addressBook3
  , testCase "Wire-encode then decoded identity" $
      roundTripWireEncodeDecode addressBook1 @?= Just addressBook1
  , testCase "Wire-decoded empty optional value should be decoded to its default" $
      roundTripWireEncodeDecode addressBook2 @?= (Just . mapDefaultPhoneType $ addressBook2)
  -- , testCase "Wire-encode then decoded identity 2" $
  --     roundTripTextEncodeDecode addressBook3 @?= Just addressBook3
  ]

addressBookQuickChecks :: TestTree
addressBookQuickChecks = testGroup "Address book QuickChecks"
  [ QC.testProperty "Address book wire-encoded then decoded identity" $
      \book -> maybe False (mapDefaultPhoneType book ==) (roundTripWireEncodeDecode book)
  -- , QC.testProperty "Address book text-encoded then decoded identity" $
  --     \book -> maybe False (mapDefaultPhoneType book ==) (roundTripTextEncodeDecode book)
  , QC.testProperty "Address book json-encoded then decoded identity" $
      \book -> maybe False (mapDefaultPhoneType book ==) (roundTripJsonEncodeDecode book)
  ]

roundTripTextEncodeDecode :: AddressBook -> Maybe AddressBook
roundTripTextEncodeDecode addressBook =
  let encoded = messagePutText addressBook
  in case messageGetText $ LB.pack encoded of
       Left _ -> Nothing
       Right result -> Just result

roundTripWireEncodeDecode :: AddressBook -> Maybe AddressBook
roundTripWireEncodeDecode addressBook =
  let encoded = messagePut addressBook
  in case messageGet encoded of
       Right (result, "") -> Just result
       _ -> Nothing

roundTripJsonEncodeDecode :: AddressBook -> Maybe AddressBook
roundTripJsonEncodeDecode addressBook =
  let encoded = J.toJSON addressBook
  in case J.fromJSON encoded of
       J.Success result -> Just result
       _ -> Nothing

addressBook1 :: AddressBook
addressBook1 =
  AddressBook {
    AddressBook'.person = Seq.fromList
      [ mkPerson "Alice" (-1) (Just "alice@example.com") $ Seq.singleton ("123-456-7890", Just HOME)
      , mkPerson "Bob" 2 Nothing $ Seq.fromList [("1-800-123-4567", Just MOBILE), ("604-291-1234", Just WORK)]
      ]
  , AddressBook'.unknown'field = defaultValue
  }

addressBook2 :: AddressBook
addressBook2 =
  AddressBook {
    AddressBook'.person = Seq.fromList
      [ mkPerson "Nobody" 2 Nothing $ Seq.singleton ("111-111-1111", Nothing)
      ]
  , AddressBook'.unknown'field = defaultValue
  }

mapDefaultPhoneType :: AddressBook -> AddressBook
mapDefaultPhoneType AddressBook{AddressBook'.person = ps, ..} =
  let people = fmap personMap ps
      personMap Person{Person'.phone = phone, ..} =
        let phoneNums = fmap phonesMap phone
            phonesMap PhoneNumber{PhoneNumber'.type' = Nothing, PhoneNumber'.number = num} =
              defaultValue{PhoneNumber'.number = num}
            phonesMap other = other
         in Person{Person'.phone = phoneNums, ..}
   in AddressBook{AddressBook'.person = people, ..}

addressBook3 :: AddressBook
addressBook3 =
  AddressBook {
    AddressBook'.person = Seq.fromList
      [ mkPerson "" 1 (Just "\550252") $ Seq.singleton ("", Just HOME)
      ]
  , AddressBook'.unknown'field = defaultValue
  }

mkPerson :: String -> Int -> Maybe String -> Seq (String, Maybe PhoneType) -> Person
mkPerson name id' email phoneNumbers =
  Person {
    Person'.name = uFromString name
  , Person'.id = fromIntegral id'
  , Person'.email = uFromString <$> email
  , Person'.phone = mkPhoneNumbers phoneNumbers
  , Person'.unknown'field = defaultValue
  }

mkPhoneNumbers :: Seq (String, Maybe PhoneType) -> Seq PhoneNumber
mkPhoneNumbers = fmap mkPhoneNumbers' where
  mkPhoneNumbers' (num, phoneType) =
    PhoneNumber {
      PhoneNumber'.number = uFromString num
    , PhoneNumber'.type' = phoneType
    , PhoneNumber'.unknown'field = defaultValue
    }

instance Arbitrary PhoneNumber where
  arbitrary = PhoneNumber <$> liftA uFromString arbitrary
                          <*> frequency [ (3, liftA Just $ elements [HOME, WORK, MOBILE])
                                        , (1, pure Nothing)]
                          <*> pure defaultValue

instance Arbitrary Person where
  arbitrary = Person <$> liftA uFromString arbitrary
                     <*> arbitrary
                     <*> frequency [ (2, liftA (Just . uFromString) arbitrary)
                                   , (1, pure Nothing)]
                     <*> liftA Seq.fromList (listOf arbitrary)
                     <*> pure defaultValue

instance Arbitrary AddressBook where
  arbitrary = AddressBook <$> liftA Seq.fromList (listOf arbitrary)
                          <*> pure defaultValue
