module Mapgen where

import Extra.Prelude

import Data.Array as Array
import Data.Map as Map
import Random (Random)
import Random as R
import Data.Traversable (scanl)
import Data.FunctorWithIndex (mapWithIndex)

import Control.Monad.Rec.Class (Step (..), tailRecM)

import Partial.Unsafe (unsafePartial)
import Control.Alt ((<|>))

type Block =
  { x:: Int, y:: Int, width:: Int, height:: Int }

type Room = Block
{-

[x] step 1
  subdivide rectangle
   - cut up large block into smaller blocks

[x] step 2
  - recursively do this again until they are all small enough

[ ] step 2.5
  - determine block adjacency

[ ] step 3
  - merge small blocks to make them bigger
  - this creates rooms

[ ] step 4
  - generate blobs
  - each blob is generated so it doesn't cut a room

[ ] step 5
  place entrance and exit on edge of map
    (and not in the same room)

[ ] step 6
  connect rooms such that
  - there is a path from start to end
  - it doesn't cross too few rooms
  - or too many rooms

[ ] step 7
  - connect some other rooms at random
    - but don't shorten the path

-}

-- start is the position of the first interior spot
-- len is the number of interior spots
type Interval = { start :: Int, len :: Int }

expandInterval :: Interval -> Array Int
expandInterval {start, len} = Array.range start (len + start - 1)

partition :: Int -> Int -> Random (Array Int)
partition min n =
  let result = case n >= 2*min+1, n >= 3*min+2 of
       false,_ -> pure [n]
       true,false -> splitinto2
       true,true -> R.chance 80 >>= case _ of
         true -> splitinto2
         false -> splitinto3
   in result

  where
    splitinto2 = do -- must split in two
         let s = n - 1
             max = n - min - 1
         split <- R.intRange min max
         let remainder = n - split - 1
         pure [split,remainder]
    splitinto3 =  do --can split into 2 or 3
         let s = n - 2 -- no room for walls?!
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
        # scanl (+) start
        # mapWithIndex (\i x -> x + i + 1)
        # Array.cons start
  pure $ Array.zipWith (\s l -> {start:s,len:l}) starts lens

subdivide :: Int -> Block -> Random (Array Block)
subdivide minSize {x,width,y,height} = do
  xIntervals <- subdivideInterval minSize {start:x,len:width}
  yIntervals <- subdivideInterval minSize {start:y,len:height}
  let mkBlock xi yi = { x: xi.start, y: yi.start, width: xi.len, height: yi.len}
  pure $ mkBlock <$> xIntervals <*> yIntervals

tooBig :: Int -> Block -> Boolean
tooBig m {width, height} = width > m || height > m


type SubdivWip = {complete :: Array Block, incomplete :: Array Block}

recursivelySubdivide :: Int -> Int -> Block -> Random (Array Block)
recursivelySubdivide minSize maxSize block = tailRecM go
  { incomplete: [block]
  , complete: []
  }
  where
    go :: SubdivWip -> Random (Step SubdivWip (Array Block))
    go {complete, incomplete} =
         let {no,yes} = Array.partition (tooBig maxSize) incomplete
             completeNew = complete <> no
          in if Array.null yes
             then pure $ Done completeNew
             else do
               arrayOfArrays <- for yes (subdivide minSize)
               let incompleteNew = Array.concat arrayOfArrays
               pure $ Loop 
                 { complete: completeNew
                 , incomplete: incompleteNew
                 }

unproduct :: Block -> {h :: Interval, v :: Interval}
unproduct {x,y,width,height} =
  { h: { start: x, len: width }
  , v: { start: y, len: height }
  }

end :: Interval -> Int
end {start, len} = start + len - 1

inInterval :: Int -> Interval -> Boolean
inInterval x i = x >= i.start && x <= end i

areKissing :: Interval -> Interval -> Boolean
areKissing a b = a.start == end b + 2
  ||             b.start == end a + 2

blocksAreAdjacent :: Block -> Block -> Boolean
blocksAreAdjacent a b =
  let sa = unproduct a
      sb = unproduct b
   in     (areKissing sa.h sb.h && not (disjoint sa.v sb.v))
       || (areKissing sa.v sb.v && not (disjoint sa.h sb.h))

roomsAreAdjacent :: Room -> Room -> Boolean
roomsAreAdjacent = blocksAreAdjacent

data Overlap = SubsetLT
  | SubsetGT
  | Equal
  | Partial Int
  | Disjoint

derive instance eqOverlap :: Eq Overlap

overlap :: Interval -> Interval -> Overlap
overlap a b = fromMaybe Disjoint $
  equal <|> subsetLT <|> subsetGT <|> partial
  where
  m x bool = if bool then Just x else Nothing
  equal = a == b # m Equal
  subsetLT = a.start >= b.start && end a <= end b # m SubsetLT
  subsetGT = a.start <= b.start && end a >= end b # m SubsetGT
  partial = a.start <= end b && b.start <= end a # m
             (Partial $ 1 + min (end a - b.start) (end b - a.start))

disjoint :: Interval -> Interval -> Boolean
disjoint a b = overlap a b == Disjoint

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex a i = unsafePartial $ Array.unsafeIndex a i

type AdjMap x =
  { edges :: Map Int (Array Int)
  , points :: Array x
  }

lookup :: forall x. Int -> AdjMap x -> x
lookup i {points} = unsafeIndex points i

-- labelled
type L b = { value :: b, label :: Int }

adjacencyMap :: forall a. Array a -> (a -> a -> Boolean) -> AdjMap a
adjacencyMap points adj = {points,edges}
  where
  pairs :: Array {a :: Int, b :: Int}
  pairs = do
    let n = Array.length points - 1
    a <- Array.range 0 n
    b <- Array.range 0 n
    guard $ a /= b
    guard $ adj (unsafeIndex points a) (unsafeIndex points b)
    pure {a,b}
  edges = foldl (\m {a,b} -> Map.insertWith (<>) a [b] m) Map.empty pairs

getEdges :: forall a. AdjMap a -> Array {a :: Int, b :: Int}
getEdges {edges} = edges
  # foldlWithIndex (\a arr targets -> arr <> do
                   b <- targets
                   guard $ a < b
                   pure {a,b}
        )
    []

listAdjacent :: forall x. AdjMap x -> L x -> Array (L x)
listAdjacent {edges, points} {label} =
  let ls = unsafeFromJust (Map.lookup label edges)
      bs = ls <#> \l -> unsafeIndex points l
   in Array.zipWith (\l b -> {label:l,value:b}) ls bs

adjacent :: forall x. AdjMap x -> L x -> L x -> Boolean
adjacent m x y = isJust $
  Array.find (\ {label} -> label == y.label)
  (listAdjacent m x)

type Conf =
  { width :: Int
  , height :: Int
  , minBlock:: Int
  , maxBlock:: Int
  }

type Result =
  { rooms :: Array Room
  , entrance :: Vector Int
  , exitCandidates :: Array (Vector Int)
  , doors :: Array Door
  }

generateMapFull :: Conf -> Random Result
generateMapFull c = do
  let startingBlock = {x:1, y:1, width: c.width - 2, height: c.height - 2 }
  rooms <- recursivelySubdivide c.minBlock c.maxBlock startingBlock
  let roomAdjacency = adjacencyMap rooms roomsAreAdjacent
  doors <- genDoors roomAdjacency
  exitCandidates <- sequence $ Array.replicate 3 (genExit rooms)
  pure $
    { rooms
    , entrance: V{x:1,y:2}
    , exitCandidates
    , doors
    }

genExit :: Array Room -> Random (Vector Int)
genExit rooms = do
  pos <- R.unsafeElement "genExit" $ mapEdgeAdjacencies rooms
  pure pos

type Door =
  { pos :: Vector Int
  , connectedRooms :: Array Int
  }

genDoors :: AdjMap Room -> Random (Array Door)
genDoors rooms = for (getEdges rooms) \ {a,b} -> do
  pos <- R.unsafeElement "genDoors" $ roomAdjacencies (lookup a rooms) (lookup b rooms)
  pure {pos, connectedRooms: [a,b]}

blockAdjacencies :: Block -> Block -> Array (Vector Int)
blockAdjacencies a b = case areKissing sa.h sb.h of
  true ->
    let ys = Array.intersect (expandInterval sa.v) (expandInterval sb.v)
        x = max a.x b.x - 1
     in ys <#> \y -> V{x,y}
  false ->
    let xs = Array.intersect (expandInterval sa.h) (expandInterval sb.h)
        y = max a.y b.y -1
     in xs <#> \x -> V{x,y}
  where
  sa = unproduct a
  sb = unproduct b

mapEdgeAdjacenciesForBlock :: Block -> Array (Vector Int)
mapEdgeAdjacenciesForBlock b =
  case b.x == 1
     || b.x + b.width == 19 of
  true -> let ys = Array.range b.y (b.y+b.height-1)
              x = if b.x == 1 then 0 else 19
          in ys <#> \y -> V{x,y}
  false -> let xs = Array.range b.x (b.x+b.width-1)
               y = if b.y == 1 then 0 else 19
           in xs <#> \x -> V{x,y}

mapEdgeAdjacencies :: Array Block -> Array (Vector Int)
mapEdgeAdjacencies blocks = Array.concat $ map mapEdgeAdjacenciesForBlock (Array.filter blockOnEdge blocks)

blockOnEdge :: Block -> Boolean
blockOnEdge b = b.x == 1
     || b.x + b.width == 18
     || b.y == 1
     || b.y + b.height == 18

roomAdjacencies :: Room -> Room -> Array (Vector Int)
roomAdjacencies = blockAdjacencies
