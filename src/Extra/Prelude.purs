module Extra.Prelude
  ( module Prelude
  , module Control.Apply
  , module Control.MonadZero
  , module Control.Monad.State
  , module Control.Monad.Trans.Class
  , module Data.DateTime.Instant
  , module Data.Either
  , module Data.Foldable
  , module Data.FoldableWithIndex
  , module Data.Group
  , module Data.Identity
  , module Data.Map
  , module Data.Maybe
  , module Data.Newtype
  , module Data.Monoid.Endo
  , module Data.Set
  , module Data.Traversable
  , module Data.Array.NonEmpty
  , module Data.TraversableWithIndex
  , module Data.Tuple
  , module Data.Unfoldable
  , module Debug.Trace
  , module Effect
  , module Extra.Math
  , module Data.Symbol
  , module Data.HeytingAlgebra

  , (|>)
  , countIf
  , filterSet
  , foldl1
  , foldr1
  , groupBy'
  , groupToMap
  , keyBy
  , keyBy'
  , pop
  , repeatedly
  , todo
  , unsafeFromJust
  , while
  ) where

import Prelude

import Control.Monad.Rec.Class (Step (..), tailRec)
import Control.Monad.State (State, get, put, modify_, modify, runState, execState, evalState, StateT)
import Control.Monad.Trans.Class (lift)
import Data.DateTime.Instant (Instant)
import Control.MonadZero (guard)
import Data.Array (groupBy, sortBy, zip, findIndex, deleteAt, index)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable
 ( class Foldable
 , all
 , null
 , length
 , elem
 , find
 , fold
 , foldr
 , foldl
 , sum
 , any
 , foldMap
 , intercalate
 , foldM
 )
import Data.FoldableWithIndex (traverseWithIndex_, forWithIndex_, foldMapWithIndex, class FoldableWithIndex, foldrWithIndex, foldlWithIndex, foldrWithIndexDefault, foldlWithIndexDefault)
import Data.Group (class Group)
import Data.HeytingAlgebra (tt,ff)
import Data.Identity (Identity (..))
import Data.Map (Map, alter, empty)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe, isJust, isNothing)
import Data.Monoid.Endo (Endo (..))
import Data.Newtype (class Newtype, unwrap, un, wrap)
import Data.Semigroup.Foldable (class Foldable1, foldMap1)
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (class Traversable, traverse, traverse_, sequence_, sequence, for, for_)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex, forWithIndex)
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Debug.Trace (trace, spy)
import Effect (Effect)
import Extra.Math (class Real, Vector(..), innerProduct, norm, toNumber, (**))
import Partial (crash)
import Partial.Unsafe (unsafePartial)
import Prim.TypeError (class Warn, Text)
import Data.Symbol (class IsSymbol, SProxy (..), reflectSymbol, reifySymbol)
import Control.Apply (class Apply, lift2, (<*>), (<*), (*>))



infixr 0 Tuple as |>

newtype SG a = SG ((a -> a -> a) -> a)

derive instance newtypeSG :: Newtype (SG a) _

instance semigroupSG :: Semigroup (SG a) where
  append (SG x) (SG y) = SG (\f -> f (x f) (y f))

foldr1 :: forall f a . Foldable1 f => (a -> a -> a) -> f a -> a
foldr1 f xs = unwrap (foldMap1 (SG <<< const)  xs) f

foldl1 :: forall f a . Foldable1 f => (a -> a -> a) -> f a -> a
foldl1 f = foldr1 (flip f)

groupBy' :: forall a. (a -> a -> Ordering) -> Array a -> Array (NonEmptyArray a)
groupBy' f xs =  groupBy ((map <<< map) (_ == EQ) f) <<< sortBy f $ xs

groupToMap :: forall f k a b
  . Foldable f => Ord k
 => { key :: a -> k
    , value :: a -> b
    , merge :: b -> b -> b
    }
 -> f a
 -> Map k b
groupToMap {key, value, merge} = foldr (\v m -> alter (combine v) (key v) m) empty
  where
  combine :: a -> Maybe b -> Maybe b
  combine v (Just v') = Just $ merge (value v) v'
  combine v Nothing = Just $ value v


filterSet :: forall a. Ord a => (a -> Boolean) -> Set a -> Set a
filterSet f = flip foldr S.empty $ \x set ->
  if f x
    then S.insert x set
    else set

todo :: forall a. Warn (Text "Not implemented") => a
todo = unsafePartial $ crash "Not implemented"

countIf :: forall a t. Foldable t => (a -> Boolean) -> t a -> Int
countIf f xs = foldr g 0 xs
  where g x acc = if f x then acc + 1 else acc

repeatedly :: forall a. Int -> (a -> a) -> a -> a
repeatedly n' f a' = tailRec go {n: n',a: a'}
  where
  go { n, a } = if (n <= 0)
                  then Done a
                  else Loop { n: n-1, a: f a }


unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust x = unsafePartial (fromJust x)


keyBy :: forall k v. Ord k => (v -> k) -> Array v -> Map k v
keyBy f vs = Map.fromFoldable $ zip (f <$> vs) vs

keyBy' :: forall k v. Ord k => (v -> Maybe k) -> Array v -> Map k v
keyBy' f vs = Map.fromFoldable $ vs >>= \v ->
  maybe [] (\k-> [Tuple k v]) (f v)

pop :: forall a. (a -> Boolean) -> State (Array a) (Maybe a)
pop f = do
  arr <- get
  case findIndex f arr of
    Nothing -> pure Nothing
    Just i -> do
      put (unsafeFromJust $ deleteAt i arr)
      pure (index arr i)

while :: forall m a. Monad m => Monoid a => m Boolean -> m a -> m a
while mp ma = mp >>= \p -> if p then (<>) <$> ma <*> while mp ma else pure mempty
