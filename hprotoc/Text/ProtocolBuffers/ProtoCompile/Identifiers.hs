{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,TypeSynonymInstances,FlexibleContexts #-}
-- | This modules colelct utility routines related to the different
-- incarnations of identifiers in the code.  The basic identifier is
-- always ASCII, but because of the self gereneted DescriptorProto
-- data structures it is stored in 'Utf8' tagged lazy bytestrings.
--
-- An 'identifier' is a non-empty ASCII string made of [a-zA-Z0-9_]
-- where the first character is never in [0-9].
--
-- A 'field' is a mangled identifer that is a valid Haskell name that
-- begins with lower case, and which may have a single quote at the
-- end if needed to avoid a reserved word.
--
-- A 'module' is a mangled identifier that is a valid Haskell name
-- that begins with upper case.  These never have a single quote.  A
-- leading _ is replaced with a leading U'_ to make a valid
-- identifier.
module Text.ProtocolBuffers.ProtoCompile.Identifiers where

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char
import Data.List
import Data.Monoid
import Data.Set(Set)
import qualified Data.Set as S
import Text.ProtocolBuffers.Basic

-- | Contains one identifier name
newtype IName a = IName {iName::a} deriving (Show,Read,Eq,Ord)
-- | Contains one module name
newtype MName a = MName {mName::a} deriving (Show,Read,Eq,Ord)
-- | Contains one field name
newtype FName a = FName {fName::a} deriving (Show,Read,Eq,Ord)
-- | '.' separated identifier which may or may start with a dot
newtype DIName a = DIName {diName :: a}
-- | Fully qualified identifier: repeated '.' then identifier.
newtype FIName a = FIName {fiName::a} deriving (Show,Read,Eq,Ord)
-- | Full Haskell module name: MNames separated by '.', ending with a module
newtype FMName a = FMName {fmName::a} deriving (Show,Read,Eq,Ord)
-- | Full Haskell field name: MNames separated by '.', ending with a field
newtype FFName a = FFName {ffName::a} deriving (Show,Read,Eq,Ord)
-- | Parsed Haskell name ending with MName
data PMName a = PMName [MName a] (MName a) deriving (Show,Read,Eq,Ord)
-- | Parsed Haskell name ending with FName
data PFName a = PFName [MName a] (FName a) deriving (Show,Read,Eq,Ord)

dotPM :: Dotted a => PMName a -> FMName a
dotPM (PMName xs (MName x)) = FMName (foldr dot x . map mName $ xs)

dotPF :: Dotted a => PFName a -> FFName a
dotPF (PFName xs (FName x)) = FFName (foldr dot x . map mName $ xs)

dotUtf8 (Utf8 a) (Utf8 b) = Utf8 (LC.append a (LC.cons '.' b))

dotString a b = a ++ ('.':b)

unull :: Utf8 -> Bool
unull = LC.null . utf8

preUtf8 :: Utf8 -> Utf8
preUtf8 (Utf8 x) = Utf8 (LC.cons 'x' x)

preString :: String -> String
preString = ('.':)

splitUtf8 :: DIName Utf8 -> [IName Utf8]
splitUtf8 = unfoldr s . utf8 . diName where
  s :: ByteString -> Maybe (IName Utf8,ByteString)
  s y | LC.null y = Nothing
      | otherwise = case U.span ('.'/=) y of
                      (a,b) | LC.null a -> s b
                            | otherwise -> Just (IName (Utf8 a),b)

splitString :: DIName String -> [IName String]
splitString = unfoldr s . diName where
  s [] = Nothing
  s ('.':xs) = s xs -- delete all '.' in the input
  s xs = Just (let (a,b) = span ('.'/=) xs in (IName a,b))

toString :: Utf8 -> String
toString = U.toString . utf8

fromString :: String -> Utf8
fromString = Utf8 . U.fromString

difi :: Dotted a => DIName a -> FIName a
difi (DIName a) = case uncons a of
                    Nothing -> FIName mempty
                    Just ('.',_) -> FIName a
                    _ -> FIName (preDot a)

{- in Text.ProtocolBuffers.Basic now
instance Monoid Utf8 where
  mempty = Utf8 mempty
  mappend (Utf8 x) (Utf8 y) = Utf8 (mappend x y)
-}

class Monoid a => Dotted a where
  uncons :: a -> Maybe (Char,a)
  preDot :: a -> a
  dot :: a -> a -> a
  splitDI :: DIName a -> [IName a]

instance Dotted Utf8 where
  uncons x = case U.uncons (utf8 x) of
               Nothing -> Nothing
               Just (c,b) -> Just (c,Utf8 b)
  preDot = preUtf8
  dot = dotUtf8
  splitDI = splitUtf8

instance Dotted String where
  uncons [] = Nothing
  uncons (x:xs) = Just (x,xs)
  preDot = preString
  dot = dotString
  splitDI = splitString

splitFI :: Dotted a => FIName a -> [IName a]
splitFI = splitDI . DIName . fiName

fqDots :: Dotted a => [IName a] -> FIName a
fqDots [] = FIName mempty
fqDots xs = FIName (preDot (foldr1 dot . map iName $ xs))

joinDots :: Dotted a => [a] -> a
joinDots [] = mempty
joinDots xs = foldr1 dot xs

-- | Right (True,_) means the input is a FIName.
-- Right (False,_) means the input is a DIName (without leading '.')
checkDIString :: String -> Either String (Bool,[IName String])
checkDIString "" = Left $ "Invalid empty identifier: "++show ""
checkDIString "." = Left $ "Invalid identifier of just a period: "++show "."
checkDIString xs | ('.':ys) <- xs = fmap ((,) True) $ parts id (span ('.'/=) ys)
                 | otherwise = fmap ((,) False) $ parts id (span ('.'/=) xs)
 where parts f ("","") = Left $ "Invalid identifier because it ends with a period: "++show xs
       parts f ("",_)  = Left $ "Invalid identifier because is contains two periods in a row: "++show xs
       parts f (_,"")  = Right (f [])
       parts f (a,b)   = parts (f . (IName a:)) (span ('.'/=) (tail b))

-- | Right (True,_) means the input is a FIName.
-- Right (False,_) means the input is a DIName (without leading '.')
checkDIUtf8 :: Utf8 -> Either String (Bool,[IName Utf8])
checkDIUtf8 s@(Utf8 xs) =
  case U.uncons xs of
    Nothing -> Left $ "Invalid empty identifier: "++show ""
    Just ('.',ys) | LC.null ys -> Left $ "Invalid identifier of just a period: "++show "."
                  | otherwise -> fmap ((,) True) $ parts id (U.span ('.'/=) ys)
    Just _ -> fmap ((,) False) $ parts id (U.span ('.'/=) xs)
 where parts f (a,b) = case (LC.null a,LC.null b) of
                         (True,True) -> Left $ "Invalid identifier because it ends with a period: "++show (toString s)
                         (True,_)    -> Left $ "Invalid identifier because is contains two periods in a row: "++show (toString s)
                         (_,True)    -> Right (f [])
                         _           -> parts (f . (IName (Utf8 a):)) (U.span ('.'/=) (U.drop 1 b))

manglePM :: Mangle a (MName x) => [a] -> PMName x
manglePM = go id where
  go ms [x] = PMName (ms []) (mangle x)
  go ms (x:xs) = go (ms . (mangle x:)) xs

manglePF :: (Mangle a (MName x),Mangle a (FName x)) => [a] -> PFName x
manglePF = go id where
  go ms [x] = PFName (ms []) (mangle x)
  go ms (x:xs) = go (ms . (mangle x:)) xs

class Mangle a b where
  mangle :: a -> b

err :: String -> a
err s = error ("Text.ProtocolBuffers.ProtoCompile.Identifiers: "++s)

instance Mangle (IName String) (MName String) where
  mangle (IName s) = MName (fixUp s)

instance Mangle (IName Utf8) (MName Utf8) where
  mangle (IName s) = MName (fromString . fixUp . toString $ s)

instance Mangle (IName Utf8) (MName String) where
  mangle (IName s) = MName (fixUp . toString $ s)

instance Mangle (IName String) (FName String) where
  mangle (IName s) = FName (fixLow s)

instance Mangle (IName Utf8) (FName Utf8) where
  mangle (IName s) = FName (fromString . fixLow . toString $ s)

instance Mangle (IName Utf8) (FName String) where
  mangle (IName s) = FName (fixLow . toString $ s)

-- make leading upper case letter or "U'_"
fixUp ('_':xs) = "U'"++xs
fixUp i@(x:xs) | isLower x =
  let x' = toUpper x
  in if isLower x' then err ("fixUp: stubborn lower case"++show i)
       else x': xs
fixUp xs = xs

-- make leading '_' or lower case letter, may end with single quote.
fixLow :: String -> String
fixLow i@(x:xs) | i `S.member` reserved = i ++ "'"
                | isUpper x = let x' = toLower x
                              in if isUpper x' then err ("fixLow: stubborn upper case: "++show i)
                                   else let i' = (x':xs)
                                        in if i' `S.member` reserved then i' ++ "'" else i'
                | otherwise = i

reserved :: Set String
reserved = S.fromDistinctAscList
  ["_"
  ,"case","class","data","default","deriving","do","else","foreign"
  ,"if","import","in","infix","infixl","infixr","instance"
  ,"let","mdo","module","newtype","of","then","type","where"
  ]
