module Mapgen where

import Extra.Prelude

import Data.Array as Array
import Random (Random)
import Random as R
import Data.Traversable (scanl)
import Data.FunctorWithIndex (mapWithIndex)

type Block =
  { x:: Int, y:: Int, width:: Int, height:: Int }

type Room = Array Block

{-

step 1
  subdivide rectangle
   - cut up large block into smaller blocks

step 2
  - recursively do this again until they are all small enough

step 3
  - merge small blocks to make them bigger
  - this creates rooms

step 4
  - generate blobs
  - each blob is generated so it doesn't cut a room

step 5
  place entrance and exit on edge of map
    (and not in the same room)

step 6
  connect rooms such that
  - there is a path from start to end
  - it doesn't cross too few rooms
  - or too many rooms

step 7
  - connect some other rooms at random
    - but don't shorten the path

-}

type Interval = { start :: Int, len :: Int }


partition :: Int -> Int -> Random (Array Int)
partition min n =
  case n < 2*min+1, n < 3*min+2 of
       false,_ -> pure [n]
       true,false -> splitinto2
       true,true -> R.chance 50 >>= case _ of
         true -> splitinto2
         false -> splitinto3
  where
    splitinto2 = do -- must split in two
         let s = n - 1
             max = n - min - 1
         split <- R.intRange min max
         let remainder = n - split - 1
         pure [split,remainder]
    splitinto3 =  do --can split into 2 or 3
         let s = n - 2
             max1 = s - 2*min
         split1 <- R.intRange min max1
         let max2 = s - split1 - min
         split2 <- R.intRange min max2
         let remainder = s - split1 - split2
         R.shuffle [split1,split2,remainder]


subdivideInterval :: Int -> Interval -> Random (Array Interval)
subdivideInterval minSize {start,len} = do
  lens <- partition minSize len
  let starts = lens
        # Array.init
        # unsafeFromJust
        # scanl (+) 0
        # mapWithIndex (+)
        # Array.cons 0
  pure $ Array.zipWith (\s l -> {start:s,len:l}) starts lens

subdivide :: Int -> Block -> Random (Array Block)
subdivide minSize {x,width,y,height} = do
  xIntervals <- subdivideInterval minSize {start:x,len:width}
  yIntervals <- subdivideInterval minSize {start:y,len:height}
  let mkBlock xi yi = { x: xi.start, y: yi.start, width: xi.len, height: yi.len}
  pure $ mkBlock <$> xIntervals <*> yIntervals






