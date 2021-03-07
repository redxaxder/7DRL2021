module Extra.Math where

import Prelude

import Data.Enum (class Enum, class BoundedEnum, Cardinality (..), defaultSucc, defaultPred, toEnum, fromEnum)
import Data.Group (class Group)
import Data.Int (toNumber) as Int
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..))
import Math (pow, sqrt)
import Data.Typelevel.Num.Sets (class Pos, toInt)
import Data.Typelevel.Undefined (undefined)


class Real a where
  toNumber :: a -> Number

instance realNumber :: Real Number where
  toNumber = identity

instance realInt :: Real Int where
  toNumber = Int.toNumber

newtype Vector a = V { x :: a, y :: a }

vec :: forall a. a -> a -> Vector a
vec x y = V{x,y}

derive newtype instance showVector :: Show a => Show (Vector a)
derive newtype instance semigroupVector :: Semigroup a => Semigroup (Vector a)
derive newtype instance monoidVector :: Monoid a => Monoid (Vector a)
derive newtype instance semiringVector :: Semiring a => Semiring (Vector a)
derive newtype instance ringVector :: Ring a => Ring (Vector a)

derive instance eqVector :: Eq a => Eq (Vector a)
derive instance ordVector :: Ord a => Ord (Vector a)
derive instance newtypeVector :: Newtype (Vector a) _

instance functorVector :: Functor Vector where
  map f (V v) = V {x: f v.x, y: f v.y }

norm :: forall a. Real a => Vector a -> Number
norm (V v) = sqrt (pow (toNumber v.x) 2.0 + pow (toNumber v.y) 2.0)

innerProduct :: forall a. Semiring a => Vector a -> Vector a -> a
innerProduct (V v) (V w) = v.x * w.x + v.y * w.y

scale :: forall a. Ring a => a -> Vector a -> Vector a
scale c (V{x,y}) = V { x: x * c, y: y * c }

infixr 8 innerProduct as **

check :: forall a. Boolean -> a -> Maybe a
check boolean x = if boolean then Just x else Nothing

newtype Zn n = Zn Int

derive instance eqzn :: Pos n => Eq (Zn n)
derive instance ordzn :: Pos n => Ord (Zn n)

instance enumZn :: Pos n => Enum (Zn n) where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance boundedEnumZn :: Pos n => BoundedEnum (Zn n) where
  cardinality = Cardinality $ toInt (undefined :: n)
  toEnum x = let r@(Zn x') = zn x
              in check (x' == x) r
  fromEnum (Zn x) = x

instance boundedZn :: Pos n => Bounded (Zn n) where
  top = zero - one
  bottom = zero

instance semigroupzn :: Pos n => Semigroup (Zn n) where
  append = add

instance groupzn :: Pos n => Group (Zn n) where
  ginverse = negate

instance monoidzn :: Pos n => Monoid (Zn n) where
  mempty = zero

instance semiringZn :: Pos n => Semiring (Zn n) where
  add (Zn x) (Zn y) = zn $ x + y
  mul (Zn x) (Zn y) = zn $ x * y
  zero = Zn 0
  one = Zn 1

instance ringZn :: Pos n => Ring (Zn n) where
  sub (Zn x) (Zn y) = zn $ x - y

zn :: forall n. Pos n => Int -> Zn n
zn x = Zn $ x `mod` (toInt (undefined :: n))

