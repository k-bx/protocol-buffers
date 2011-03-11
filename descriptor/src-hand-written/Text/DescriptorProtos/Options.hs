{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
{-| This module provides a less verbose API for accessing the options
    of the various descriptor messages types.  There are seven
    different option types.  The EnumValueOptions are not currently
    settable in the proto file.  To access extension keys the
    descendKey functions are provided as the descend functions ignore
    them.  The 'toDP', 'toEP', 'toSP' are type-specific descents that
    are demonstrated in the the tests below.  They are useful in that
    they provide more information for the type-checker.  The 'toFP',
    'toEVP', and 'toMP' are fully type-specific descents but are
    needed to make the tests below type-check, though they could have
    been used in 'test4', test5', and 'test7'.

> import Text.DescriptorProtos.Options
> import Text.DescriptorProtos(fileDescriptorProto)
> 
> test1 :: D.FileOptions
> test1 = options fileDescriptorProto
> 
> test2 :: Maybe D.MessageOptions
> test2 = return fileDescriptorProto >>= descend "FieldDescriptorProto" >>= return . options
> 
> test3 :: Maybe D.EnumOptions
> test3 = return fileDescriptorProto >>= toDP "FieldDescriptorProto" >>= descend "Type" >>= return . options
> 
> test4 :: Maybe D.EnumValueOptions
> test4 = return fileDescriptorProto >>= toDP "FieldDescriptorProto" >>= toEP "Type" >>= descend "TYPE_DOUBLE" >>= return . options
> 
> test5 :: Maybe D.FieldOptions
> test5 = return fileDescriptorProto >>= toDP "DescriptorProto" >>= toDP "ExtensionRange" >>= descend "start" >>= return . options
> 
> test6 :: Maybe D.ServiceOptions
> test6 = return fileDescriptorProto >>= descend "ImaginaryService" >>= return . options
> 
> test7 :: Maybe D.MethodOptions
> test7 = return fileDescriptorProto >>= toSP "ImaginaryService" >>= descend "ImaginaryMethod" >>= return . options
-}
module Text.DescriptorProtos.Options(descend,descendKey
                                    ,toDP,toEP,toSP,toFP,toEVP,toMP
                                    ,NameAndOptions(name,options),DescendClass(descend'),DescendKey(descendKey')
                                    ,D.FileDescriptorProto,D.DescriptorProto,D.EnumDescriptorProto
                                    ,D.EnumValueDescriptorProto,D.FieldDescriptorProto
                                    ,D.ServiceDescriptorProto,D.MethodDescriptorProto
                                    ,D.EnumOptions,D.EnumValueOptions,D.FieldOptions
                                    ,D.FileOptions,D.MessageOptions,D.MethodOptions,D.ServiceOptions) where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D(ExtensionRange(ExtensionRange))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.ExtensionRange(ExtensionRange(..))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D(EnumDescriptorProto(EnumDescriptorProto))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D.EnumDescriptorProto(EnumDescriptorProto(..))
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D(EnumValueDescriptorProto)
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D.EnumValueDescriptorProto(EnumValueDescriptorProto(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D(FieldDescriptorProto)
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D.FieldDescriptorProto(FieldDescriptorProto(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type      as D.Type(Type(..))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto)
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..))
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D(MethodDescriptorProto)
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D.MethodDescriptorProto(MethodDescriptorProto(..))
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D(ServiceDescriptorProto)
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D.ServiceDescriptorProto(ServiceDescriptorProto(..))
import qualified Text.DescriptorProtos.UninterpretedOption            as D(UninterpretedOption)
import qualified Text.DescriptorProtos.UninterpretedOption            as D.UninterpretedOption(UninterpretedOption(..))
import qualified Text.DescriptorProtos.UninterpretedOption.NamePart   as D(NamePart(NamePart))
import qualified Text.DescriptorProtos.UninterpretedOption.NamePart   as D.NamePart(NamePart(..))
import qualified Text.DescriptorProtos.EnumOptions      as D(EnumOptions)
import qualified Text.DescriptorProtos.EnumOptions      as D.EnumOptions(EnumOptions(..))
import qualified Text.DescriptorProtos.EnumValueOptions as D(EnumValueOptions)
import qualified Text.DescriptorProtos.EnumValueOptions as D.EnumValueOptions(EnumValueOptions(..))
import qualified Text.DescriptorProtos.FieldOptions     as D(FieldOptions)
import qualified Text.DescriptorProtos.FieldOptions     as D.FieldOptions(FieldOptions(..))
import qualified Text.DescriptorProtos.FileOptions      as D(FileOptions)
import qualified Text.DescriptorProtos.FileOptions      as D.FileOptions(FileOptions(..))
import qualified Text.DescriptorProtos.MessageOptions   as D(MessageOptions)
import qualified Text.DescriptorProtos.MessageOptions   as D.MessageOptions(MessageOptions(..))
import qualified Text.DescriptorProtos.MethodOptions    as D(MethodOptions)
import qualified Text.DescriptorProtos.MethodOptions    as D.MethodOptions(MethodOptions(..))
import qualified Text.DescriptorProtos.ServiceOptions   as D(ServiceOptions)
import qualified Text.DescriptorProtos.ServiceOptions   as D.ServiceOptions(ServiceOptions(..))

import Text.DescriptorProtos(fileDescriptorProto) -- for testing

import Data.Maybe(listToMaybe)
import Text.ProtocolBuffers(fromString,Utf8,getVal,Seq)
import qualified Data.Foldable as F(toList)

class DescendClass a c where
  descend' :: a -> Utf8 -> (Maybe c)

descend :: DescendClass a c => String -> a -> Maybe c
descend = flip descend' . fromString

class DescendKey a where
  descendKey' :: a -> Utf8 -> Maybe D.FieldDescriptorProto

descendKey :: DescendKey a => String -> a -> Maybe D.FieldDescriptorProto
descendKey = flip descendKey' . fromString

class NameAndOptions a opt | a -> opt, opt -> a where
  name :: a -> Utf8
  options :: a -> opt

-- helper
search :: (NameAndOptions a b) => Seq a -> Utf8 -> Maybe a
search s n = listToMaybe [ m | m <- F.toList s, n == name m ]

instance DescendClass D.DescriptorProto D.DescriptorProto where
  descend' dp = search (D.DescriptorProto.nested_type dp)

instance DescendClass D.DescriptorProto D.EnumDescriptorProto where
  descend' dp = search (D.DescriptorProto.enum_type dp)

instance DescendClass D.DescriptorProto D.FieldDescriptorProto where
  descend' dp = search (D.DescriptorProto.field dp)

instance DescendClass D.FileDescriptorProto D.DescriptorProto where
  descend' dp = search (D.FileDescriptorProto.message_type dp)

instance DescendClass D.FileDescriptorProto D.EnumDescriptorProto where
  descend' dp = search (D.FileDescriptorProto.enum_type dp)

instance DescendClass D.FileDescriptorProto D.ServiceDescriptorProto where
  descend' dp = search (D.FileDescriptorProto.service dp)

instance DescendClass D.EnumDescriptorProto D.EnumValueDescriptorProto where
  descend' dp = search (D.EnumDescriptorProto.value dp)

instance DescendClass D.ServiceDescriptorProto D.MethodDescriptorProto where
  descend' dp = search (D.ServiceDescriptorProto.method dp)

instance DescendKey D.FileDescriptorProto where
  descendKey' dp = search (D.FileDescriptorProto.extension dp)

instance DescendKey D.DescriptorProto where
  descendKey' dp = search (D.DescriptorProto.extension dp)

instance NameAndOptions D.FileDescriptorProto D.FileOptions where
  name = flip getVal D.FileDescriptorProto.name
  options = flip getVal D.FileDescriptorProto.options

instance NameAndOptions D.DescriptorProto D.MessageOptions where
  name = flip getVal D.DescriptorProto.name
  options = flip getVal D.DescriptorProto.options

instance NameAndOptions D.FieldDescriptorProto D.FieldOptions where
  name = flip getVal D.FieldDescriptorProto.name
  options = flip getVal D.FieldDescriptorProto.options

instance NameAndOptions D.ServiceDescriptorProto D.ServiceOptions where
  name = flip getVal D.ServiceDescriptorProto.name
  options = flip getVal D.ServiceDescriptorProto.options

instance NameAndOptions D.MethodDescriptorProto D.MethodOptions where
  name = flip getVal D.MethodDescriptorProto.name
  options = flip getVal D.MethodDescriptorProto.options

instance NameAndOptions D.EnumValueDescriptorProto D.EnumValueOptions where
  name = flip getVal D.EnumValueDescriptorProto.name
  options = flip getVal D.EnumValueDescriptorProto.options

instance NameAndOptions D.EnumDescriptorProto D.EnumOptions where
  name = flip getVal D.EnumDescriptorProto.name
  options = flip getVal D.EnumDescriptorProto.options

toDP :: DescendClass a D.DescriptorProto => String -> a -> Maybe D.DescriptorProto
toDP s a = descend' a (fromString s)

toEP :: DescendClass a D.EnumDescriptorProto => String -> a -> Maybe D.EnumDescriptorProto
toEP s a = descend' a (fromString s)

toSP :: String -> D.FileDescriptorProto -> Maybe D.ServiceDescriptorProto
toSP s a = descend' a (fromString s)

toFP :: String -> D.DescriptorProto -> Maybe D.FieldDescriptorProto
toFP s a = descend' a (fromString s)

toEVP :: String -> D.EnumDescriptorProto -> Maybe D.EnumValueDescriptorProto
toEVP s a = descend' a (fromString s)

toMP :: String -> D.ServiceDescriptorProto -> Maybe D.MethodDescriptorProto
toMP s a = descend' a (fromString s)

test1 :: D.FileOptions
test1 = options fileDescriptorProto

test2 :: Maybe D.MessageOptions
test2 = return fileDescriptorProto >>= descend "FieldDescriptorProto" >>= return . options

test3 :: Maybe D.EnumOptions
test3 = return fileDescriptorProto >>= toDP "FieldDescriptorProto" >>= descend "Type" >>= return . options

test4 :: Maybe D.EnumValueOptions
test4 = return fileDescriptorProto >>= toDP "FieldDescriptorProto" >>= toEP "Type" >>= descend "TYPE_DOUBLE" >>= return . options

test5 :: Maybe D.FieldOptions
test5 = return fileDescriptorProto >>= toDP "DescriptorProto" >>= toDP "ExtensionRange" >>= descend "start" >>= return . options

test6 :: Maybe D.ServiceOptions
test6 = return fileDescriptorProto >>= descend "ImaginaryService" >>= return . options

test7 :: Maybe D.MethodOptions
test7 = return fileDescriptorProto >>= toSP "ImaginaryService" >>= descend "ImaginaryMethod" >>= return . options
