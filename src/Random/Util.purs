module Random.Util where

import Extra.Prelude

import Random.Gen (Random, intRange)
import Data.Array as Array
import Data.List.Lazy as Lazy

import Control.Monad.ST as ST
import Control.Monad.ST (ST)
import Partial.Unsafe (unsafePartial)
import Data.Array.ST (STArray, peek, poke, unsafeFreeze, withArray)

shuffle :: forall a. Array a -> Random (Array a)
shuffle array = unsafePartial $ do
  let n = Array.length array
  p <- makePermutation n
  pure $ ST.run (withArray (shuffleHelper p) array :: forall h. ST h (Array a))
  where
  shuffleHelper :: forall h.  Partial => Lazy.List Int -> STArray h a -> ST h (Array a)
  shuffleHelper permutation arr = do
     traverseWithIndex_ (\ i p -> swap i (i+p) arr) permutation
     unsafeFreeze arr
  swap :: forall h. Partial => Int -> Int -> STArray h a -> ST h Unit
  swap i j arr = do
    i_ <- fromJust <$> peek i arr
    j_ <- fromJust <$> peek j arr
    void $ poke i j_ arr
    void $ poke j i_ arr

makePermutation :: Int -> Random (Lazy.List Int)
makePermutation n =
  if n == 0 then pure mempty
    else Lazy.cons <$> intRange 0 (n - 1) <*> makePermutation (n-1)
