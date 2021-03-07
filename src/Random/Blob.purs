module Random.Blob
  ( Blob
  , Ints (..)
  , Doubles (..)
  , fromInts
  , toInts
  , toDoubles
  , merge
  , perturb
  ) where

import Extra.Prelude

import Node.Buffer
  ( Buffer
  , BufferValueType (Int32LE)
  , concat
  , create
  , read
  , write
  )
import Effect.Unsafe (unsafePerformEffect)
import Data.Int (floor)

import Random.Hash (Algorithm (SHA256), hash)

-- 256 bit (32 byte) binary blob
newtype Blob = Blob Buffer

merge :: Blob -> Blob -> Blob
merge (Blob b1) (Blob b2) = Blob $ hash SHA256
  (unsafePerformEffect $ concat [b1, b2])

perturb :: Blob -> Blob
perturb (Blob b) = Blob (hash SHA256 b)

data Ints = Ints Int Int Int Int Int Int Int Int

writeInt32LE :: Int -> Int -> Buffer -> Effect Unit
writeInt32LE d i b = write Int32LE (toNumber d) i b

fromInts :: Ints -> Blob
fromInts (Ints a b c d e f g h) = unsafePerformEffect $ do
  buffer <- create 32
  writeInt32LE a 0 buffer
  writeInt32LE b 4 buffer
  writeInt32LE c 8 buffer
  writeInt32LE d 12 buffer
  writeInt32LE e 16 buffer
  writeInt32LE f 20 buffer
  writeInt32LE g 24 buffer
  writeInt32LE h 28 buffer
  pure $ Blob buffer

readInt32LE :: Int -> Buffer -> Effect Int
readInt32LE i b = floor <$> read Int32LE i b

toInts :: Blob -> Ints
toInts (Blob b) = unsafePerformEffect $ Ints
  <$> readInt32LE 0 b
  <*> readInt32LE 4 b
  <*> readInt32LE 8 b
  <*> readInt32LE 12 b
  <*> readInt32LE 16 b
  <*> readInt32LE 20 b
  <*> readInt32LE 24 b
  <*> readInt32LE 28 b


foreign import readDouble :: Int -> Buffer -> Effect Number

data Doubles = Doubles Number Number Number Number

toDoubles :: Blob -> Doubles
toDoubles (Blob b) = unsafePerformEffect $ Doubles
  <$> readDouble 0 b
  <*> readDouble 8 b
  <*> readDouble 16 b
  <*> readDouble 24 b
