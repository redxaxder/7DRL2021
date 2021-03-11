module Data.RevMap
  ( RevMap
  , empty
  , insert
  , delete
  , lookup
  , lookupReverse
  , fromFoldable
  , fromFoldableWithIndex
  , values
  , uniqueValues
  )
  where

import Prelude

import Data.Map as Map
import Data.Map (Map)
import Data.Tuple (Tuple (..))
import Data.Foldable
  ( class Foldable
  , foldr
  , foldl
  , foldMap
  )

import Data.FoldableWithIndex
  ( class FoldableWithIndex
  , foldrWithIndex
  , foldlWithIndex
  , foldMapWithIndex
  )


import Data.Maybe (fromMaybe, Maybe (..))
import Data.Array as Array

-- A multimap is a map with an index for (multi) preimage lookup

data RevMap k v =
  RevMap (Map k v) (Map v (Array k))

empty :: forall k v. Ord k => Ord v => RevMap k v
empty = RevMap Map.empty Map.empty

insert :: forall k v. Ord k => Ord v => k -> v -> RevMap k v -> RevMap k v
insert k v (RevMap f g) = RevMap (Map.insert k v f) $
       -- add a new reverse lookup entry
       Map.insertWith (<>) v (Array.singleton k) g
       -- remove dangling reverse reference
       # cleanupDeleted k (Map.lookup k f)

lookup :: forall k v. Ord k => k -> RevMap k v -> Maybe v
lookup k (RevMap l _) = Map.lookup k l

delete :: forall k v. Ord k => Ord v => k -> RevMap k v -> RevMap k v
delete k (RevMap l r) = RevMap (Map.delete k l)
  (cleanupDeleted k (Map.lookup k l) r)

lookupReverse :: forall k v. Ord v => v -> RevMap k v -> Maybe (Array k)
lookupReverse v (RevMap _ r) = Map.lookup v r

fromFoldable :: forall f k v. Ord k => Ord v => Foldable f => f (Tuple k v) -> RevMap k v
fromFoldable xs = foldl (\m (Tuple k v) -> insert k v m) empty xs

fromFoldableWithIndex :: forall f k v. Ord k => Ord v => FoldableWithIndex k f => f v -> RevMap k v
fromFoldableWithIndex = foldlWithIndex (\k m v -> insert k v m) empty

values :: forall k v. Ord k => RevMap k v -> Array v
values (RevMap l _) = Array.fromFoldable $ Map.values l

uniqueValues :: forall k v. Ord k => RevMap k v -> Array v
uniqueValues (RevMap _ r) =
  foldlWithIndex (\v acc _ -> Array.cons v acc) [] r


instance foldableRevMap :: Foldable (RevMap k) where
  foldr f z (RevMap l _) = foldr f z l
  foldl f z (RevMap l _) = foldl f z l
  foldMap f (RevMap l _) = foldMap f l

instance foldableWithIndexRevMap :: FoldableWithIndex k (RevMap k) where
  foldrWithIndex f z (RevMap l _) = foldrWithIndex f z l
  foldlWithIndex f z (RevMap l _) = foldlWithIndex f z l
  foldMapWithIndex f (RevMap l _) = foldMapWithIndex f l


--------------------------------------------------------------------------------
-- helper functions that aren't part of the API because they dont work on their
-- own
---------------------------------------------------------------------------------

cleanupDeleted :: forall k v. Ord k => Ord v => k -> Maybe v -> Map v (Array k) -> Map v (Array k)
cleanupDeleted k mayv m = fromMaybe m do
     v <- mayv
     pure $ Map.alter (\xs -> xs >>= del k) v m

del ::forall k. Eq k => k -> Array k -> Maybe (Array k)
del x xs = case Array.elemIndex x xs of
                Nothing -> Just xs
                Just i -> Array.deleteAt i xs


---------------------------------------------------------------------------------





{- Equivalents of Map api to implement when convenient:
  , showTree
  , isEmpty
  , singleton
  , checkValid
  , insertWith
  , lookup
  , lookupLE
  , lookupLT
  , lookupGE
  , lookupGT
  , findMin
  , findMax
  , foldSubmap
  , submap
  , fromFoldable
  , fromFoldableWith
  , fromFoldableWithIndex
  , toUnfoldable
  , toUnfoldableUnordered
  , delete
  , pop
  , member
  , alter
  , update
  , keys
  , values
  , union
  , unionWith
  , unions
  , intersection
  , intersectionWith
  , difference
  , isSubmap
  , size
  , filterWithKey
  , filterKeys
  , filter
  , mapMaybeWithKey
  , mapMaybe
  , catMaybes

-}
