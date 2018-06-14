{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module AesonDemo where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Data
import           Data.Maybe                 (fromJust)
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Storable.ATS
import           GHC.Generics

data Some a = Some a
            | None
            deriving (Typeable, Data, Generic, ToJSON, FromJSON, ATSStorable)

data Person a b = Person {
      name :: a
    , age  :: b
    } deriving (Typeable, Data, Generic, ToJSON, FromJSON, ATSStorable)

type YoungEnglishSpeakingPerson = Person CString Int

decodeFail :: String -> Person String Int
decodeFail = fromJust . decode . BSL.pack

strToCStr :: Person String a -> IO (Person CString a)
strToCStr (Person s x) = Person <$> toCString s <*> pure x

decode_json :: CString -> IO (Ptr YoungEnglishSpeakingPerson)
decode_json cstr = g =<< peekCString cstr
    where g = writePtr <=< strToCStr . decodeFail

foreign export ccall decode_json :: CString -> IO (Ptr YoungEnglishSpeakingPerson)
