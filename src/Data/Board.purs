module Data.Board where

import Extra.Prelude
import Data.Array as Array
import Data.Tuple as Tuple
import Data.Set as Set
import Framework.Direction (move, directions8)

data OrganSize = OrganSize Int Int
data OrganType = Hp
data Organ = Organ OrganSize OrganType

newtype BoardCoord = BoardCoord (Vector Int)
newtype Board = Board
  { organs :: Array (Tuple Organ BoardCoord)
  -- , organIndex :: Map BoardCoord Organ
  , injuries :: Set BoardCoord
  }

derive instance ordBoardCoord :: Ord BoardCoord
derive instance eqBoardCoord :: Eq BoardCoord

derive instance newtypeBoard :: Newtype Board _

injureBoard :: Vector Int -> Board -> Board
injureBoard v (Board b) = Board b {injuries = Set.insert (BoardCoord v) b.injuries}

isValidBoardCoord :: BoardCoord -> Boolean
isValidBoardCoord (BoardCoord (V{x,y})) = x >= 0
  && x <= 6
  && y >= 0
  && y <= 6

getOrganAtPosition :: Board -> BoardCoord -> Maybe Organ
getOrganAtPosition (Board {organs}) p =
  Tuple.fst <$> Array.find (isInside p) organs

isInside :: BoardCoord -> Tuple Organ BoardCoord -> Boolean
isInside (BoardCoord (V{x: px,y: py}))
         (Tuple (Organ (OrganSize w h) _) (BoardCoord (V{x,y}))) =
  x <= px
  && px < x + w
  && y <= py
  && py < y + h

isHpOrgan :: Organ -> Boolean
isHpOrgan (Organ _ Hp) = true

getClue :: BoardCoord -> Board -> Clue
getClue (BoardCoord p) b =
  let neighbors = Array.filter isValidBoardCoord
        (map BoardCoord $ move <$> directions8 <*> pure p)
      hpOrgans = Array.length $ Array.filter isHpOrgan
        (Array.mapMaybe (getOrganAtPosition b) neighbors)
   in if hpOrgans > 0
      then HpClue hpOrgans
      else EmptyClue

hpCount :: Board -> Int
hpCount board = Array.length $ Array.filter isHp (intactOrgans board)
  where
  isHp (Organ _ Hp) = true
  -- isHp _ = false

intactOrgans :: Board -> Array Organ
intactOrgans (Board board) =
  Tuple.fst <$> Array.filter noInjuries board.organs
  where
  noInjuries (Tuple (Organ (OrganSize w h) _) (BoardCoord (V {x, y}))) =
    let xmin = x
        xmax = w + x - 1
        ymin = y
        ymax = y + h - 1
    in not $ any (\(BoardCoord (V i)) ->
       i.x >= xmin
       && i.x <= xmax
       && i.y >= ymin
       && i.y <= ymax) board.injuries

data Clue =
  HpClue Int -- Health only
  | ArmorClue Int -- Armor only
  | MixedClue Int -- HealthAndArmor
  | ConcealedClue
  | EmptyClue
