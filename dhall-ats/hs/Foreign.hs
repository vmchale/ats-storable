{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign where

import           Control.Monad
import           Data.Bifunctor
import           Data.Data
import qualified Data.Text.Lazy       as TL
import           Dhall
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable.ATS

data Option a = Some a
              | None
              deriving (Generic, Interpret, Functor, Data, ATSStorable)

data Pair a b = Pair { first :: a, second :: b }
    deriving (Generic, Interpret, Data, ATSStorable)

instance Bifunctor Pair where
    bimap f g (Pair x y) = Pair (f x) (g y)

type Product = Pair CInt CInt

readDhall :: FilePath -> IO (Option Product)
readDhall p = fmap (join bimap g) <$> input auto (TL.pack p)
    where g :: Integer -> CInt
          g = fromIntegral

read_dhall :: CString -> IO (Ptr (Option Product))
read_dhall cStr = do
    str <- peekCString cStr
    x <- readDhall str
    writePtr x

foreign export ccall read_dhall :: CString -> IO (Ptr (Option Product))
