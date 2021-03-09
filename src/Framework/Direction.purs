module Framework.Direction where

import Extra.Prelude

import Extra.Math (zn, Zn (..))

import Data.Enum (class Enum, class BoundedEnum, upFromIncluding)
import Data.Typelevel.Num.Reps (D8)

data Spin = Clockwise | Widdershins

newtype Direction = Direction (Zn D8)

instance showDireciton :: Show Direction where
  show (Direction (Zn d)) = case d of
                     0 -> "right"
                     1 -> "upRight"
                     2 -> "up"
                     3 -> "upLeft"
                     4 -> "left"
                     5 -> "downLeft"
                     6 -> "down"
                     _ -> "downRight"

right :: Direction
right     = fromInt 0

upRight :: Direction
upRight   = fromInt 1

up :: Direction
up        = fromInt 2

upLeft :: Direction
upLeft    = fromInt 3

left :: Direction
left      = fromInt 4

downLeft :: Direction
downLeft  = fromInt 5

down :: Direction
down      = fromInt 6

downRight :: Direction
downRight = fromInt 7

directions4 :: Array Direction
directions4 = [right, up, left, down]

directions8 :: Array Direction
directions8 = upFromIncluding right

fromInt :: Int -> Direction
fromInt x = Direction (zn x)

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction
derive newtype instance enumDirection :: Enum Direction
derive newtype instance boundedDirection :: Bounded Direction
derive newtype instance boundedEnumDirection :: BoundedEnum Direction
derive newtype instance semigroupDirection :: Semigroup Direction
derive newtype instance monoidDirecton :: Monoid Direction
derive newtype instance groupDirecton :: Group Direction

rotate4 :: Spin -> Direction -> Direction
rotate4 Widdershins d = fromInt 2 <> d
rotate4 Clockwise   d = fromInt 6 <> d

rotate8 :: Spin -> Direction -> Direction
rotate8 Widdershins d = fromInt 1 <> d
rotate8 Clockwise d = fromInt 7 <> d

move :: forall a. Ring a => Direction -> Vector a -> Vector a
move (Direction (Zn d)) (V p) = case d of
  0 -> V p{ x = p.x + one }                  -- right
  1 -> V p{ x = p.x + one , y = p.y - one }  -- upright
  2 -> V p{ y = p.y - one }                  -- up
  3 -> V p{ y = p.y - one , x = p.x - one }  -- upleft
  4 -> V p{ x = p.x - one }                  -- left
  5 -> V p{ x = p.x - one , y = p.y + one }  -- downleft
  6 -> V p{ y = p.y + one }                  -- down
  _ -> V p{ y = p.y + one , x = p.x + one }  -- downright

-- this is the direction to move p in to get q
-- unMove p (move dir p) == Just dir
unMove :: Vector Int -> Vector Int -> Maybe Direction
unMove p q = case q - p of
  (V { x:  1, y:  0}) -> Just right
  (V { x:  1, y: -1}) -> Just upRight
  (V { x:  0, y: -1}) -> Just up
  (V { x: -1, y: -1}) -> Just upLeft
  (V { x: -1, y:  0}) -> Just left
  (V { x: -1, y:  1}) -> Just downLeft
  (V { x:  0, y:  1}) -> Just down
  (V { x:  1, y:  1}) -> Just downRight
  _ -> Nothing
