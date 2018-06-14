{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE DeriveDataTypeable      #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE IncoherentInstances     #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}

-- | You'll probably want to use this module to simply derive an 'ATSStorable' instance. To do so:
--
-- > {-# LANGUAGE DeriveGeneric      #-}
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > {-# LANGUAGE DeriveAnyClass     #-}
-- >
-- > data MyType a = MyType a
-- >     deriving (Generic, Data, ATSStorable)
module Foreign.Storable.ATS
    ( ATSStorable (..)
    , AsCString (..)
    ) where

import Data.Bool (bool)
import           Control.Composition
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import           Data.Data
import           Data.Foldable
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import qualified Foreign.Storable      as C
import           GHC.Generics

deriving instance Data CChar
deriving instance Data CInt

class AsCString a where
    toCString :: a -> IO CString

instance AsCString String where
    toCString = newCString

instance AsCString T.Text where
    toCString = newCString . T.unpack

instance AsCString TL.Text where
    toCString = newCString . TL.unpack

instance AsCString BS.ByteString where
    toCString = flip BS.useAsCString pure

instance AsCString BSL.ByteString where
    toCString = flip BS.useAsCString pure . BSL.toStrict

data ATSTypeConfig = ATSTypeConfig { _i         :: Word8 -- ^ Index of the particular constructor
                                   , n          :: Word8 -- ^ Number of constructors for the type
                                   , _recursive :: Bool -- ^ Whether or not the type is self-recursive
                                   , _special   :: Bool -- ^ Flag to be set when the type has exactly one self-recursive type.
                                   }

class Storable' f where

    sizeOf' :: f a -> Int

    alignment' :: f a -> Int

    peek' :: ATSTypeConfig -> Ptr (f a) -> IO (f a)

    poke' :: ATSTypeConfig -> Ptr (f a) -> f a -> IO ()

    pokeByteOff' :: ATSTypeConfig -> Ptr (f a) -> Int -> f a -> IO ()
    pokeByteOff' cfg = poke' cfg .* plusPtr

    peekByteOff' :: ATSTypeConfig -> Ptr (f a) -> Int -> IO (f a)
    peekByteOff' cfg = peek' cfg .* plusPtr

instance Storable' U1 where
    sizeOf' = pure 0
    alignment' = pure 0
    poke' _ _ = pure undefined
    peek' _ _ = pure undefined

instance Storable' V1 where
    peek' = undefined
    alignment' = undefined
    poke' = undefined
    sizeOf' = undefined

instance (Storable' a, Storable' b) => Storable' (a :*: b) where
    sizeOf' _ = sizeOf' (undefined :: a x) + sizeOf' (undefined :: b x)
    alignment' _ = gcd (alignment' (undefined :: a x)) (alignment' (undefined :: b x))
    peek' cfg ptr = do
        a <- peek' cfg (castPtr ptr)
        (a :*:) <$> peekByteOff' cfg (castPtr ptr) (sizeOf' a)
    poke' cfg ptr (a :*: b) =
        poke' cfg (castPtr ptr) a >>
        pokeByteOff' cfg (castPtr ptr) (sizeOf' a) b

numConstructors :: (Data a) => a -> Int
numConstructors x = subtract 1 . length $ takeWhile (/= ix) cs
    where ix = toConstr x
          cs = dataTypeConstrs (dataTypeOf x)

sumHelper :: Storable' f => ATSTypeConfig
                         -> Ptr a -- ^ Pointer we want to write our value at
                         -> f b -- ^ Value to be written
                         -> IO ()
sumHelper cfg@(ATSTypeConfig _ _ _ True) ptr val = do
    bytesPtr <- mallocBytes (sizeOf' val)
    poke' cfg bytesPtr val
    C.poke (castPtr ptr) bytesPtr
sumHelper cfg@(ATSTypeConfig _ _ True False) ptr val = do
    bytesPtr <- mallocBytes (sizeOf' val)
    poke' cfg bytesPtr val
    C.pokeByteOff (castPtr ptr) 1 bytesPtr
sumHelper cfg@(ATSTypeConfig _ _ False False) ptr val =
    pokeByteOff' cfg (castPtr ptr) 1 val

ptrSize :: Int
ptrSize = C.sizeOf (undefined :: (Ptr Word8))

-- The rules for storing a type in ATS are somewhat complex, so it bears writing
-- them down here.
--
-- 1. For a type which may be recursive (including all universally quantified
-- types), the variable type must be heap-allocated.
--
-- 2. In the specific case of a (possibly recursive) sum type with two
-- constructors, one of which is empty, we may simply use a null pointer.
--
-- 3. For other types, we simply tag the constructor number and use a boxed
-- (stack-allocated) type.
--
-- Product types are a good deal simpler.

instance (Storable' a, Storable' b) => Storable' (a :+: b) where
    sizeOf' _ = 1 + ptrSize
    alignment' _ = 1

    peek' cfg@(ATSTypeConfig _ _ True True) ptr = do
        i' <- C.peek (castPtr ptr) :: IO Word8
        bool
            (R1 <$> (peek' cfg (castPtr ptr) :: IO (b x)))
            (L1 <$> (peek' cfg (castPtr ptr) :: IO (a x)))
            (i' /= 0)
    peek' _ _ = undefined

    poke' cfg@ATSTypeConfig{} ptr (L1 val) = fold
        [ C.poke (castPtr ptr) (n cfg)
        , sumHelper cfg ptr val ]
    poke' cfg@ATSTypeConfig{} ptr (R1 val) = fold
        [ C.poke (castPtr ptr) (n cfg)
        , sumHelper cfg ptr val ]

instance (C.Storable a) => Storable' (K1 i a) where
    sizeOf' _ = C.sizeOf (undefined :: a)
    alignment' _ = C.alignment (undefined :: a)
    peek' _ ptr = pure K1 <*> C.peek (castPtr ptr)
    poke' _ ptr (K1 val) = C.poke (castPtr ptr) val

instance (Storable' a) => Storable' (M1 i c a) where
    sizeOf' _ = sizeOf' (undefined :: a x)
    alignment' _ = alignment' (undefined :: a x)
    peek' cfg ptr = pure M1 <*> peek' cfg (castPtr ptr)
    poke' cfg ptr (M1 val) = poke' cfg (castPtr ptr) val

index' :: Data a => a -> Word8
index' = fromIntegral . constrIndex . toConstr

count' :: Data a => a -> Word8
count' = fromIntegral . numConstructors

atsCfg' :: (Recurse a, Data a) => a -> ATSTypeConfig
atsCfg' a = ATSTypeConfig (index' a) (count' a) (selfRecursive a) (isSpecial a)

instance (Generic a, Storable' (Rep a), Data a, Recurse a) => C.Storable a where
    sizeOf _ = (sizeOf' . from) (undefined :: a)
    alignment = C.sizeOf
    poke ptr x = poke' (atsCfg' x) (castPtr ptr) (from x)
    peek = fmap to . peek' (atsCfg' (undefined :: a)) . castPtr

class Recurse' f where

    selfRecursive' :: f a -> Bool
    isSpecial' :: f a -> Bool

instance Recurse' V1 where
    selfRecursive' = undefined
    isSpecial' = undefined

instance Recurse' U1 where
    selfRecursive' = pure False
    isSpecial' = pure False

instance (Recurse' a, Recurse' b) => Recurse' (a :+: b) where

    selfRecursive' _ = selfRecursive' (undefined :: a x) || selfRecursive' (undefined :: b x)
    isSpecial' _ = selfRecursive' (undefined :: a x) /= selfRecursive' (undefined :: b x)

instance (Recurse' a, Recurse' b) => Recurse' (a :*: b) where

    selfRecursive' _ = selfRecursive' (undefined :: a x) || selfRecursive' (undefined :: b x)
    isSpecial' _ = selfRecursive' (undefined :: a x) || selfRecursive' (undefined :: b x)

instance Recurse' a => Recurse' (M1 i c a) where

    selfRecursive' (M1 val) = selfRecursive' val
    isSpecial' (M1 val) = isSpecial' val

instance Recurse' (K1 i a) where

    selfRecursive' = pure True
    isSpecial' = pure True

class Recurse a where

    selfRecursive :: a -> Bool

    isSpecial :: a -> Bool

instance (Generic a, Recurse' (Rep a)) => Recurse a where

    selfRecursive = selfRecursive' . from

    isSpecial = isSpecial' . from

class ATSStorable a where

    -- | Read a value at a pointer.
    readPtr :: C.Storable a => Ptr a -> IO a
    readPtr = C.peek

    -- | Write a value to a pointer.
    writePtr :: C.Storable a => a -> IO (Ptr a)
    writePtr val = do
        ptr <- mallocBytes (C.sizeOf val)
        C.poke ptr val
        pure ptr
