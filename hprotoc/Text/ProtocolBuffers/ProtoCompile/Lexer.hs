{-# OPTIONS -cpp #-}
{-# LINE 1 "Lexer.x" #-}

{-# OPTIONS_GHC -Wwarn #-}
module Text.ProtocolBuffers.ProtoCompile.Lexer (Lexed(..), alexScanTokens,getLinePos)  where

import Control.Monad.Error()
import Codec.Binary.UTF8.String(encode)
import qualified Data.ByteString.Lazy as L
import Data.Char(ord,isHexDigit,isOctDigit,toLower)
import Data.Word(Word8)
import Numeric(readHex,readOct,readDec,readSigned,readFloat)
import Debug.Trace


#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.



import qualified Data.ByteString.Lazy.Char8 as ByteString









-- -----------------------------------------------------------------------------
-- The input type

{-# LINE 35 "templates/wrappers.hs" #-}


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  ByteString.ByteString)        -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,cs) | ByteString.null cs = Nothing
                     | otherwise = let c   = ByteString.head cs
                                       cs' = ByteString.tail cs
                                       p'  = alexMove p c
                                    in p' `seq` cs' `seq` Just (c, (p', c, cs'))


-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad

{-# LINE 162 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

{-# LINE 251 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper

{-# LINE 273 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

{-# LINE 297 "templates/wrappers.hs" #-}

{-# LINE 322 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.

{-# LINE 339 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version


--alexScanTokens :: ByteString -> [token]
alexScanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (ByteString.take (fromIntegral len) str) : go inp'



-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

alex_base :: Array Int Int
alex_base = listArray (0,77) [-8,-3,2,109,0,-26,7,8,9,10,-19,-17,-21,-20,-7,81,83,105,139,115,176,0,253,275,329,366,0,441,464,502,0,356,577,161,263,599,609,619,0,0,335,336,742,743,744,767,823,745,847,894,916,940,962,746,337,768,973,1034,1045,769,1107,1118,1129,1140,1175,907,1203,1280,1357,1434,1511,1569,1627,0,-52,-58,0,0]

alex_table :: Array Int Int
alex_table = listArray (0,1882) [0,3,2,3,3,3,2,2,2,2,2,2,2,2,2,2,11,-1,-1,-1,-1,11,11,10,3,10,54,8,5,2,4,42,76,76,2,11,76,19,71,15,23,17,17,17,17,17,17,17,17,17,73,76,74,76,0,0,0,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,76,0,76,0,67,0,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,76,0,76,2,2,2,2,2,12,0,0,0,0,6,34,0,18,18,18,18,18,18,18,18,18,18,2,0,0,0,0,0,0,0,0,0,34,35,18,18,18,18,18,18,18,18,18,18,22,16,16,16,16,16,16,16,16,16,0,35,0,0,0,0,0,0,0,0,0,35,34,0,18,18,18,18,18,18,18,18,18,18,0,0,0,0,0,0,0,0,0,35,0,35,33,33,33,33,33,33,33,33,33,33,0,75,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,0,35,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,34,0,24,24,24,24,24,24,24,24,31,31,32,32,32,32,32,32,32,32,32,32,34,35,24,24,24,24,24,24,24,24,31,31,0,0,-1,-1,-1,0,0,0,28,0,0,35,-1,-1,-1,0,0,0,0,0,0,35,0,0,0,0,0,0,0,0,28,0,0,0,0,0,0,39,39,0,28,39,34,35,24,24,24,24,24,24,24,24,31,31,0,0,0,0,0,0,0,0,28,0,0,35,0,0,0,34,0,31,31,31,31,31,31,31,31,31,31,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,35,0,45,57,57,35,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,35,0,0,0,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,27,27,27,27,27,27,27,27,27,0,0,0,0,0,0,0,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,0,0,0,0,0,0,0,27,27,27,27,27,27,0,0,0,27,27,27,27,27,27,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,27,27,27,27,27,27,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,32,32,32,32,32,32,32,32,32,32,0,0,0,0,0,0,0,36,0,36,0,35,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,35,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,-1,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0,-1,-1,-1,-1,-1,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,0,0,0,0,0,0,0,-1,-1,-1,0,39,39,39,39,39,0,0,0,0,0,0,46,46,46,46,46,46,46,46,46,46,39,39,0,0,40,0,0,46,46,46,46,46,46,48,49,49,49,49,49,49,49,-1,0,0,0,0,0,0,0,0,0,-1,45,45,45,45,45,0,0,46,46,46,46,46,46,-1,0,0,0,0,0,0,0,44,0,-1,0,45,57,57,39,0,0,0,0,0,0,0,0,47,47,47,47,47,47,47,47,47,47,0,0,0,0,0,39,44,47,47,47,47,47,47,-1,50,50,50,50,50,50,50,50,0,-1,0,0,-1,0,0,0,0,0,0,0,45,-1,-1,0,0,47,47,47,47,47,47,-1,0,0,0,0,0,0,39,0,0,0,0,0,45,-1,39,51,51,51,51,51,51,51,51,-1,0,0,0,0,39,0,0,0,0,0,0,-1,0,52,52,52,52,52,52,52,52,-1,-1,0,0,0,0,0,39,0,0,0,-1,0,0,45,0,53,53,53,53,53,53,53,53,0,0,0,57,0,39,0,0,0,0,0,39,45,0,53,53,53,53,53,53,53,53,0,0,0,58,58,58,58,58,58,58,58,58,58,0,45,0,-1,0,0,0,58,58,58,58,58,58,-1,-1,0,0,0,0,0,0,0,0,45,-1,0,0,0,0,0,0,0,0,0,57,0,0,41,0,58,58,58,58,58,58,0,0,0,39,0,0,60,61,61,61,61,61,61,61,0,0,0,59,59,59,59,59,59,59,59,59,59,0,0,0,0,-1,0,0,59,59,59,59,59,59,0,-1,-1,0,0,0,56,0,0,0,57,0,-1,-1,0,0,0,0,0,0,0,57,0,-1,-1,39,59,59,59,59,59,59,0,0,-1,0,39,0,56,62,62,62,62,62,62,62,62,39,0,0,63,63,63,63,63,63,63,63,39,-1,0,64,64,64,64,64,64,64,64,-1,0,0,65,65,65,65,65,65,65,65,0,0,0,57,0,0,0,0,0,0,0,0,0,39,57,0,0,0,0,0,0,0,0,0,0,57,0,65,65,65,65,65,65,65,65,0,57,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,72,0,68,68,68,68,68,68,68,68,68,68,0,0,0,0,0,0,57,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,0,0,0,0,68,0,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,72,0,68,68,68,68,68,68,68,68,68,68,0,0,0,0,0,0,0,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,0,0,0,0,68,0,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,72,0,68,68,68,68,68,68,68,68,68,68,0,0,0,0,0,0,0,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,0,0,0,0,68,0,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,72,0,70,70,70,70,70,70,70,70,70,70,0,0,0,0,0,0,0,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,0,0,0,0,70,0,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,72,0,70,70,70,70,70,70,70,70,70,70,0,0,0,0,0,0,0,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,0,0,0,0,70,0,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,0,0,0,0,66,0,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,0,0,0,0,69,0,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,1882) [-1,9,10,11,12,13,9,10,11,12,13,9,10,11,12,13,42,10,10,10,10,42,42,42,32,42,34,35,47,32,47,39,40,41,32,42,44,45,46,47,48,49,50,51,52,53,54,55,56,57,102,59,110,61,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,-1,93,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,-1,125,9,10,11,12,13,42,-1,-1,-1,-1,47,46,-1,48,49,50,51,52,53,54,55,56,57,32,-1,-1,-1,-1,-1,-1,-1,-1,-1,46,69,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,-1,69,-1,-1,-1,-1,-1,-1,-1,-1,-1,101,46,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,101,-1,69,48,49,50,51,52,53,54,55,56,57,-1,105,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,101,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,46,69,48,49,50,51,52,53,54,55,56,57,-1,-1,0,0,0,-1,-1,-1,88,-1,-1,69,10,10,10,-1,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,-1,-1,-1,88,-1,-1,-1,-1,-1,-1,34,34,-1,120,39,46,101,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,120,-1,-1,69,-1,-1,-1,46,-1,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,-1,69,-1,92,92,92,101,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,101,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,-1,-1,-1,97,98,99,100,101,102,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,97,98,99,100,101,102,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,43,-1,45,-1,69,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,-1,101,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,0,0,0,0,0,-1,-1,-1,-1,-1,10,10,10,10,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,-1,-1,-1,-1,-1,-1,-1,10,10,10,-1,39,39,39,39,39,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,34,34,-1,-1,39,-1,-1,65,66,67,68,69,70,48,49,50,51,52,53,54,55,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,92,92,92,92,92,-1,-1,97,98,99,100,101,102,0,-1,-1,-1,-1,-1,-1,-1,88,-1,10,-1,92,92,92,39,-1,-1,-1,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,39,120,65,66,67,68,69,70,0,48,49,50,51,52,53,54,55,-1,10,-1,-1,0,-1,-1,-1,-1,-1,-1,-1,92,0,10,-1,-1,97,98,99,100,101,102,10,-1,-1,-1,-1,-1,-1,39,-1,-1,-1,-1,-1,92,0,34,48,49,50,51,52,53,54,55,10,-1,-1,-1,-1,39,-1,-1,-1,-1,-1,-1,0,-1,48,49,50,51,52,53,54,55,10,0,-1,-1,-1,-1,-1,39,-1,-1,-1,10,-1,-1,92,-1,48,49,50,51,52,53,54,55,-1,-1,-1,92,-1,39,-1,-1,-1,-1,-1,34,92,-1,48,49,50,51,52,53,54,55,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,92,-1,0,-1,-1,-1,65,66,67,68,69,70,10,0,-1,-1,-1,-1,-1,-1,-1,-1,92,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,-1,34,-1,97,98,99,100,101,102,-1,-1,-1,34,-1,-1,48,49,50,51,52,53,54,55,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,0,-1,-1,65,66,67,68,69,70,-1,10,0,-1,-1,-1,88,-1,-1,-1,92,-1,10,0,-1,-1,-1,-1,-1,-1,-1,92,-1,10,0,34,97,98,99,100,101,102,-1,-1,10,-1,34,-1,120,48,49,50,51,52,53,54,55,34,-1,-1,48,49,50,51,52,53,54,55,34,0,-1,48,49,50,51,52,53,54,55,10,-1,-1,48,49,50,51,52,53,54,55,-1,-1,-1,92,-1,-1,-1,-1,-1,-1,-1,-1,-1,34,92,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,92,-1,48,49,50,51,52,53,54,55,-1,92,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,46,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,92,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,77) [77,-1,-1,-1,-1,13,7,7,9,9,13,14,13,13,13,-1,-1,-1,-1,-1,21,-1,-1,-1,-1,26,-1,-1,-1,30,-1,-1,-1,-1,-1,-1,-1,38,-1,-1,43,55,43,43,43,43,43,43,43,43,43,43,43,43,55,55,55,55,55,55,55,55,55,55,55,55,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,77) [[],[],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[(AlexAccSkip)],[],[],[],[],[],[(AlexAcc (alex_action_14))],[(AlexAccPred  (alex_action_2) (alexRightContext 20)),(AlexAccPred  (alex_action_5) (alexRightContext 37)),(AlexAcc (alex_action_6))],[(AlexAccPred  (alex_action_2) (alexRightContext 20)),(AlexAccPred  (alex_action_5) (alexRightContext 37)),(AlexAcc (alex_action_6))],[(AlexAccPred  (alex_action_2) (alexRightContext 20)),(AlexAccPred  (alex_action_5) (alexRightContext 37)),(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_14))],[],[(AlexAccSkip)],[(AlexAccPred  (alex_action_3) (alexRightContext 25)),(AlexAccPred  (alex_action_5) (alexRightContext 37)),(AlexAcc (alex_action_7))],[(AlexAccPred  (alex_action_3) (alexRightContext 25)),(AlexAccPred  (alex_action_5) (alexRightContext 37)),(AlexAcc (alex_action_7))],[(AlexAccPred  (alex_action_3) (alexRightContext 25)),(AlexAccPred  (alex_action_5) (alexRightContext 37)),(AlexAcc (alex_action_7))],[],[(AlexAccSkip)],[(AlexAccPred  (alex_action_4) (alexRightContext 29)),(AlexAcc (alex_action_8))],[],[],[(AlexAccSkip)],[(AlexAccPred  (alex_action_5) (alexRightContext 37)),(AlexAcc (alex_action_9))],[(AlexAccPred  (alex_action_5) (alexRightContext 37)),(AlexAcc (alex_action_9))],[(AlexAccPred  (alex_action_5) (alexRightContext 37)),(AlexAcc (alex_action_9))],[],[],[],[],[(AlexAccSkip)],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_14))],[],[],[],[],[],[],[],[],[],[],[],[(AlexAcc (alex_action_14))],[],[],[],[],[],[],[],[],[],[],[],[(AlexAcc (alex_action_11))],[(AlexAcc (alex_action_11))],[(AlexAcc (alex_action_11))],[(AlexAcc (alex_action_11))],[(AlexAcc (alex_action_11))],[(AlexAcc (alex_action_13))],[],[(AlexAcc (alex_action_12))],[],[],[(AlexAcc (alex_action_13))],[(AlexAcc (alex_action_14))]]
{-# LINE 58 "Lexer.x" #-}

line :: AlexPosn -> Int
line (AlexPn _byte lineNum _col) = lineNum
{-# INLINE line #-}

data Lexed = L_Integer !Int !Integer
           | L_Double !Int !Double
           | L_Name !Int !L.ByteString
           | L_String !Int !L.ByteString !L.ByteString
           | L !Int !Char
           | L_Error !Int !String
  deriving (Show,Eq)

getLinePos :: Lexed -> Int
getLinePos x = case x of
                 L_Integer i _ -> i
                 L_Double  i _ -> i
                 L_Name    i _ -> i
                 L_String  i _ _ -> i
                 L         i _ -> i
                 L_Error   i _ -> i

-- 'errAt' is the only access to L_Error, so I can see where it is created with pos
errAt pos msg =  L_Error (line pos) $ "Lexical error (in Text.ProtocolBuffers.Lexer): "++ msg ++ ", at "++see pos where
  see (AlexPn char lineNum col) = "character "++show char++" line "++show lineNum++" column "++show col++"."
dieAt msg pos _s = errAt pos msg
wtfAt pos s = errAt pos $ "unknown character "++show c++" (decimal "++show (ord c)++")"
  where (c:_) = ByteString.unpack s

{-# INLINE mayRead #-}
mayRead :: ReadS a -> String -> Maybe a
mayRead f s = case f s of [(a,"")] -> Just a; _ -> Nothing

-- Given the regexps above, the "parse* failed" messages should be impossible.
parseDec pos s = maybe (errAt pos "Impossible? parseDec failed")
                       (L_Integer (line pos)) $ mayRead (readSigned readDec) (ByteString.unpack s)
parseOct pos s = maybe (errAt pos "Impossible? parseOct failed")
                       (L_Integer (line pos)) $ mayRead (readSigned readOct) (ByteString.unpack s)
parseHex pos s = maybe (errAt pos "Impossible? parseHex failed")
                       (L_Integer (line pos)) $ mayRead (readSigned (readHex . drop 2)) (ByteString.unpack s)
parseDouble pos s = maybe (errAt pos "Impossible? parseDouble failed")
                          (L_Double (line pos)) $ mayRead (readSigned readFloat) (ByteString.unpack s)
-- The sDecode of the string contents may fail
parseStr pos s = let middle = ByteString.init . ByteString.tail $ s
                 in either (errAt pos) (L_String (line pos) middle . L.pack)
                    . sDecode . ByteString.unpack $ middle

-- Here s is always "-inf" matched by @ninf
parseNinf pos s = trace ("parseNinf got "++show (pos,s)) $
                  L_Name (line pos) s

parseName pos s = L_Name (line pos) s
parseChar pos s = L (line pos) (ByteString.head s)

-- Generalization of concat . unfoldr to monadic-Either form:
op :: ( [Char] -> Either String (Maybe ([Word8],[Char]))) -> [Char] -> Either String [Word8]
op one = go id where
  go f cs = case one cs of
              Left msg -> Left msg
              Right Nothing -> Right (f [])
              Right (Just (ws,cs')) -> go (f . (ws++)) cs'

-- Put this mess in the lexer, so the rest of the code can assume
-- everything is saner.  The input is checked to really be "Char8"
-- values in the range [0..255] and to be c-escaped (in order to
-- render binary information printable).  This decodes the c-escaping
-- and returns the binary data as Word8.
-- 
-- A decoding error causes (Left msg) to be returned.
sDecode :: [Char] -> Either String [Word8]
sDecode = op one where
  one :: [Char] -> Either String (Maybe ([Word8],[Char]))
  one ('\\':xs) = unescape xs
  one (x:xs) = do x' <- checkChar8 x
                  return $ Just (x',xs)  -- main case of unescaped value
  one [] = return Nothing
  unescape [] = Left "cannot understand a string that ends with a backslash"
  unescape ys | 1 <= len =
      case mayRead readOct oct of
        Just w -> do w' <- checkByte w
                     return $ Just (w',rest)
        Nothing -> Left $ "failed to decode octal sequence "++ys
    where oct = takeWhile isOctDigit (take 3 ys)
          len = length oct
          rest = drop len ys
  unescape (x:ys) | 'x' == toLower x && 1 <= len =
      case mayRead readHex hex of
        Just w -> do w' <- checkByte w
                     return $ Just (w',rest)
        Nothing -> Left $ "failed to decode hex sequence "++ys
    where hex = takeWhile isHexDigit (take 2 ys)
          len = length hex
          rest = drop len ys          
  unescape ('u':ys) | ok =
      case mayRead readHex hex of
        Just w -> do w' <- checkUnicode w
                     return $ Just (w',rest)
        Nothing -> Left $ "failed to decode 4 char unicode sequence "++ys
    where ok = all isHexDigit hex && 4 == length hex
          (hex,rest) = splitAt 4 ys
  unescape ('U':ys) | ok =
      case mayRead readHex hex of
        Just w -> do w' <- checkUnicode w
                     return $ Just (w',rest)
        Nothing -> Left $ "failed to decode 8 char unicode sequence "++ys
    where ok = all isHexDigit hex && 8 == length hex
          (hex,rest) = splitAt 8 ys
  unescape (x:xs) = do x' <- decode x
                       return $ Just ([x'],xs)
  decode :: Char -> Either String Word8
  decode 'a' = return 7
  decode 'b' = return 8
  decode 't' = return 9
  decode 'n' = return 10
  decode 'v' = return 11
  decode 'f' = return 12
  decode 'r' = return 13
  decode '\"' = return 34
  decode '\'' = return 39
  decode '?' = return 63    -- C99 rule : "\?" is '?'
  decode '\\' = return 92
  decode x | toLower x == 'x' = Left "cannot understand your 'xX' hexadecimal escaped value"
  decode x | toLower x == 'u' = Left "cannot understand your 'uU' unicode UTF-8 hexadecimal escaped value"
  decode _ = Left "cannot understand your backslash-escaped value"
  checkChar8 :: Char -> Either String [Word8]
  checkChar8 c | (0 <= i) && (i <= 255) = Right [toEnum i]
               | otherwise = Left $ "found Char out of range 0..255, value="++show (ord c)
    where i = fromEnum c
  checkByte :: Integer -> Either String [Word8]
  checkByte i | (0 <= i) && (i <= 255) = Right [fromInteger i]
              | otherwise = Left $ "found Oct/Hex Int out of range 0..255, value="++show i
  checkUnicode :: Integer -> Either String [Word8]
  checkUnicode i | (0 <= i) && (i <= 127) = Right [fromInteger i]
                 | i <= maxChar = Right $ encode [ toEnum . fromInteger $ i ]
                 | otherwise = Left $ "found Unicode Char out of range 0..0x10FFFF, value="++show i
    where maxChar = toInteger (fromEnum (maxBound ::Char)) -- 0x10FFFF


alex_action_2 =  parseDec 
alex_action_3 =  parseOct 
alex_action_4 =  parseHex 
alex_action_5 =  parseDouble 
alex_action_6 =  dieAt "decimal followed by invalid character" 
alex_action_7 =  dieAt "octal followed by invalid character" 
alex_action_8 =  dieAt "hex followed by invalid character" 
alex_action_9 =  dieAt "floating followed by invalid character" 
alex_action_10 =  parseStr 
alex_action_11 =  parseName 
alex_action_12 =  parseNinf 
alex_action_13 =  parseChar 
alex_action_14 =  wtfAt 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 35 "templates/GenericTemplate.hs" #-}

{-# LINE 45 "templates/GenericTemplate.hs" #-}

{-# LINE 66 "templates/GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 87 "templates/GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 98 "templates/GenericTemplate.hs" #-}
quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input len, _) ->



		AlexSkip input len

	(AlexLastAcc k input len, _) ->



		AlexToken input len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = check_accs (alex_accept `quickIndex` (s))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		base   = alexIndexInt32OffAddr alex_base s
		(ord_c) = ord c
		offset = (base + ord_c)
		check  = alexIndexInt16OffAddr alex_check offset
		
		new_s = if (offset >= (0)) && (check == ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	case new_s of 
	    (-1) -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (len + (1)) 
			new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (len)
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (len)
	check_accs (AlexAccPred a pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastAcc a input (len)
	check_accs (AlexAccSkipPred pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastSkip input (len)
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (i) = i
