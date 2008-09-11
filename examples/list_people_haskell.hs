module Main where

import Control.Monad(when)
import qualified Data.ByteString.Lazy as L(readFile)
import qualified Data.Foldable as F(forM_)
import System.Environment(getArgs,getProgName)
import qualified Data.ByteString.Lazy.UTF8 as U(toString)
import qualified System.IO.UTF8 as U(putStrLn)

import Text.ProtocolBuffers(messageGet,utf8,isSet,getVal)

import AddressBookProtos.AddressBook(person)
import AddressBookProtos.Person as Person(id,name,email,phone)
import AddressBookProtos.Person.PhoneNumber(number,type')
import AddressBookProtos.Person.PhoneType(PhoneType(MOBILE,HOME,WORK))

outLn = U.putStrLn . U.toString . utf8

listPeople address_book =
  F.forM_ (person address_book) $ \person -> do
    putStr "Person ID: " >> print (Person.id person)
    putStr "  Name: " >> outLn (name person)
    when (isSet person email) $ do
      putStr "  E-mail address: " >> outLn (getVal person email)
    F.forM_ (phone person) $ \phone_number -> do
      putStr $ case getVal phone_number type' of
                 MOBILE -> "  Mobile phone #: "
                 HOME   -> "  Home phone #: "
                 WORK   -> "  Work phone #: "
      outLn (number phone_number)

main = do
  args <- getArgs
  f <- case args of
         [file] -> L.readFile file
         _ -> getProgName >>= \self -> error $ "Usage "++self++" ADDRESS_BOOK_FILE"
  case messageGet f of
    Left msg -> error ("Failed to parse address book.\n"++msg)
    Right (address_book,_) -> listPeople address_book
