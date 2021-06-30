{-# LANGUAGE OverloadedStrings #-}
module Text.ProtocolBuffers.Test.Tests.Films
  ( playlistQuickChecks
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC

import qualified Data.Aeson as J
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Applicative (liftA)
import Data.Proxy

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
playlistQuickChecks = quickCheckTests "Film" (Proxy :: Proxy Playlist)

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
  arbitrary = Talent <$> liftA Seq.fromList (listOf arbitrary)
                     <*> arbitrary

instance Arbitrary Film where
  arbitrary = Film <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance Arbitrary Playlist where
  arbitrary = resize 8 $
    Playlist <$> liftA Seq.fromList (listOf arbitrary)
             <*> arbitrary

