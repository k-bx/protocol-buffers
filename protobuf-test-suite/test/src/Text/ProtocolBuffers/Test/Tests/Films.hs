{-# LANGUAGE OverloadedStrings #-}
module Text.ProtocolBuffers.Test.Tests.Films
  ( playlistQuickChecks
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Applicative (liftA)

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.TextMessage
import Text.ProtocolBuffers.WireMessage

import Text.ProtocolBuffers.Test.QuickCheck

import qualified HSCodeGen.Films.Film          as Film'
import qualified HSCodeGen.Films.Playlist      as Playlist'
import qualified HSCodeGen.Films.Talent        as Talent'
import qualified HSCodeGen.Films.Talent.Person as Person'

import HSCodeGen.Films.Film (Film(..))
import HSCodeGen.Films.Playlist (Playlist(..))
import HSCodeGen.Films.Talent (Talent(..))
import HSCodeGen.Films.Talent.Person (Person(..))

playlistQuickChecks :: TestTree
playlistQuickChecks = testGroup "Film playlist QuickChecks"
  [ QC.testProperty "Film playlist wire-encoded then decoded identity" $
      \films -> maybe False (films ==) (roundTripWireEncodeDecode films)
  ]

roundTripWireEncodeDecode :: Playlist -> Maybe Playlist
roundTripWireEncodeDecode playlist =
  let encoded = messagePut playlist
  in case messageGet encoded of
       Right (result, "") -> Just result
       _ -> Nothing

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary
                     <*> liftA uFromString arbitrary
                     <*> arbitraryFilmography
                     <*> arbitrary
    where
      arbitraryFilmography =
        liftA Seq.fromList $
        sized $ \n ->
          scale (`div` 2) $ vector n

instance Arbitrary Talent where
  arbitrary = Talent <$> arbitrary
                     <*> arbitrary

instance Arbitrary Film where
  arbitrary = Film <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance Arbitrary Playlist where
  arbitrary = resize 8 $
    Playlist <$> liftA Seq.fromList (listOf arbitrary)
             <*> arbitrary

