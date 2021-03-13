module Solver where

import Extra.Prelude

import Data.Set as Set
import Data.List (List)
import Data.List as List
import Data.Array as Array
import Data.Tuple as Tuple
import Data.Map as Map



floodFill :: forall v. Ord v => v -> (v -> Array v) -> Set v
floodFill start expand = _.expanded $ flip loop
  { frontier: List.singleton start
  , expanded: Set.empty
  } $ floodFillStep expand

reachable :: forall v. Ord v => v -> v -> (v -> Array v) -> Boolean
reachable start end expand = Tuple.snd $
  flip (loopUntil (\{ frontier, expanded } -> end `elem` frontier))
  { frontier: List.singleton start
  , expanded: Set.empty
  } $ floodFillStep expand

type FloodFillStepData v = { frontier :: List v, expanded :: Set v }

floodFillStep :: forall v. Ord v => (v -> Array v) -> FloodFillStepData v -> Maybe (FloodFillStepData v)
floodFillStep expand {frontier, expanded} = do
     { head, tail } <- List.uncons frontier
     let newVertices = Array.filter (not <<< flip Set.member expanded) $ expand head
         frontier' = foldr List.Cons tail newVertices
         expanded' = Set.insert head expanded
     pure $ { frontier: frontier', expanded: expanded' }

distanceMap :: forall v. Ord v => v -> (v -> Array v) -> Map v Int
distanceMap start expand = _.expanded $ flip loop
  { frontier: List.singleton {v:start,d:0}
  , expanded: Map.empty
  } $ distanceMapStep expand

type DistanceMapStepData v =
  { frontier :: List {v::v,d::Int}
  , expanded :: Map v Int
  }

distanceMapStep :: forall v.
  Ord v => (v -> Array v) -> DistanceMapStepData v -> Maybe (DistanceMapStepData v)
distanceMapStep expand {frontier, expanded} = do
  { head: head@{v,d}, tail } <- List.uncons frontier
  let newNodes = expand v
               # Array.filter (not <<< flip Map.member expanded)
               # map (\w -> {v:w,d:d+1})
      frontier' = foldr List.Cons tail newNodes
      expanded' = Map.insert v d expanded
  pure $ { frontier: frontier', expanded: expanded' }


loop :: forall a.  (a -> Maybe a) -> a -> a
loop next seed = maybe seed (loop next) (next seed)

loopUntil :: forall a. (a -> Boolean) -> (a -> Maybe a) -> a -> Tuple a Boolean
loopUntil stop next seed = let succeeded = stop seed
                            in if succeeded
                               then Tuple seed succeeded
                               else maybe (Tuple seed false) (loopUntil stop next) (next seed)




