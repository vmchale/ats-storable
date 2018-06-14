{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}

import           Data.Data            (Data)
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable.ATS
import           GHC.Generics         (Generic)
import           Test.Hspec

foreign import ccall unsafe something :: Ptr (Option Product)
foreign import ccall unsafe something_else :: Ptr (Tri CInt)

data Option a = Some a
              | None
              deriving (Show, Eq, Generic, Functor, Data, ATSStorable)

data Tri a = First a
           | Second
           | Third
           deriving (Show, Eq, Generic, Functor, Data, ATSStorable)

data Pair a b = Pair { _first :: a, _second :: b }
    deriving (Show, Eq, Generic, Data, ATSStorable)

type Product = Pair CInt CInt

somethingVal :: IO (Option Product)
somethingVal = readPtr something

somethingElseVal :: IO (Tri CInt)
somethingElseVal = readPtr something_else

main :: IO ()
main = hspec $ parallel $
    describe "readPtr" $ do
        it "should work on a combined sum/product type" $
            somethingVal >>= (`shouldBe` (Some (Pair 1 6)))
        it "should work on a combined sum/product type" $
            (pure $ pendingWith "not yet") somethingElseVal -- somethingElseVal >>= (`shouldBe` (First 2))
