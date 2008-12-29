module Main where

import Control.Monad
import Text.ProtocolBuffers(messageGet,messagePut,Utf8(..),defaultValue)
import Text.ProtocolBuffers.Header(Utf8(..))
import qualified Data.ByteString.Lazy as L(readFile,writeFile,null)
import System
import Data.Char
import Data.Maybe(fromMaybe)
import qualified Data.ByteString.Lazy.UTF8 as U(fromString)
import qualified System.IO.UTF8 as U(getLine,putStr)
import Data.Sequence((|>),empty)

import AddressBookProtos.AddressBook
import AddressBookProtos.Person as Person
import AddressBookProtos.Person.PhoneNumber
import AddressBookProtos.Person.PhoneType


mayRead s = case reads s of ((x,_):_) -> Just x ; _ -> Nothing

main = do
  args <- getArgs
  file <- case args of
            [file] -> return file
            _ -> getProgName >>= \self -> error $ "Usage "++self++" ADDRESS_BOOK_FILE"
  f <- L.readFile file
  newBook <- case messageGet f of
               Left msg -> error ("Failed to parse address book.\n"++msg)
               Right (address_book,_) -> promptForAddress address_book
  seq newBook $ L.writeFile file (messagePut newBook)

inStr prompt = U.putStr prompt >> getLine
inUtf8 prompt = inStr prompt >>= return . Utf8 . U.fromString

promptForAddress :: AddressBook -> IO AddressBook
promptForAddress address_book = do
  p <- liftM3 (\i n e -> defaultValue { Person.id = i, name = n, email = if L.null (utf8 e) then Nothing else Just e})
              (fmap read  $ inStr "Enter person ID number: ")
              (inUtf8 "Enter name: ")
              (inUtf8 "Enter email address (blank for none): ")
  p' <- getPhone p
  return (address_book { person = person address_book |> p' })

getPhone :: Person -> IO Person
getPhone p' = do
  n <- inUtf8 "Enter a phone number (or leave blank to finish): "
  if L.null(utf8 n)
    then return p'
    else do t <- fmap (mayRead . map toUpper) (inStr "Is this a mobile, home, or work phone? ")
            getPhone (p' { phone = phone p' |> defaultValue { number = n, type' = t }})
