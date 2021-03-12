module Data.Board where

import Extra.Prelude
import Data.Array as Array
import Data.Tuple as Tuple
import Data.RevMap (RevMap)
import Data.RevMap as RevMap
import Data.Set as Set
import Random as R
import Framework.Direction (move, directions8)

data OrganSize = OrganSize Int Int
data OrganType = Hp | PlayerHeartLarge

data Organ = Organ OrganSize OrganType

derive instance eqOrganSize :: Eq OrganSize
derive instance ordOrganSize :: Ord OrganSize
derive instance eqOrganType :: Eq OrganType
derive instance ordOrganType :: Ord OrganType
derive instance eqOrgan :: Eq Organ
derive instance ordOrgan :: Ord Organ

type BoardCoord = Vector Int

type InternalOrgan = Tuple Organ BoardCoord

type OrganBag = RevMap (Vector Int) InternalOrgan

emptyBag :: OrganBag
emptyBag = RevMap.empty

insertOrgan :: BoardCoord -> Organ -> OrganBag -> OrganBag
insertOrgan pos organ bag =
  let io = Tuple organ pos
      coveredPositions = extent organ pos
   in foldl (\b v -> RevMap.insert v io b) bag coveredPositions

--we can only insert an organ here if it doesnt overlap any existing ones
canInsertOrgan :: BoardCoord -> Organ -> OrganBag -> Boolean
canInsertOrgan pos organ@(Organ (OrganSize w h) _) bag =
  let coveredPositions = extent organ pos
      lookups = coveredPositions <#> \p -> RevMap.lookup p bag
   in not $ any isJust lookups

removeOrganAt :: BoardCoord -> OrganBag -> OrganBag
removeOrganAt p bag = foldl (\b x -> RevMap.delete x b) bag $ organExtent p bag

organAt :: BoardCoord -> OrganBag -> Maybe InternalOrgan
organAt = RevMap.lookup

-- checks position for an organ and returns all the spaces it occupies
-- if there is no organ there, the array is empty
organExtent :: BoardCoord -> OrganBag -> Array BoardCoord
organExtent v orgs = fromMaybe [] do
  organ <- RevMap.lookup v orgs
  RevMap.lookupReverse organ orgs

extent :: Organ -> BoardCoord -> Array BoardCoord
extent (Organ (OrganSize w h) _) pos = do
  x <- Array.range 0 (w - 1)
  y <- Array.range 0 (h - 1)
  pure (pos + V{x,y})


organArray :: OrganBag -> Array InternalOrgan
organArray = RevMap.uniqueValues

newtype Board = Board
  { organs :: OrganBag
  , injuries :: Set BoardCoord
  }

derive instance newtypeBoard :: Newtype Board _

injureBoard :: Vector Int -> Board -> Board
injureBoard v (Board b) = Board b {injuries = Set.insert v b.injuries}

isValidBoardCoord :: BoardCoord -> Boolean
isValidBoardCoord (V{x,y}) = x >= 0
  && x <= 5
  && y >= 0
  && y <= 5

getOrganAtPosition :: Board -> BoardCoord -> Maybe Organ
getOrganAtPosition (Board {organs}) p = Tuple.fst <$> organAt p organs

isInside :: BoardCoord -> Tuple Organ BoardCoord -> Boolean
isInside (V{x: px,y: py})
         (Tuple (Organ (OrganSize w h) _) (V{x,y})) =
  x <= px
  && px < x + w
  && y <= py
  && py < y + h

isHpOrgan :: Organ -> Boolean
isHpOrgan (Organ _ Hp) = true
isHpOrgan (Organ _ PlayerHeartLarge) = true

getClue :: BoardCoord -> Board -> Clue
getClue p b =
  let neighbors = Array.filter isValidBoardCoord
        (move <$> directions8 <*> pure p)
      hpOrgans = Array.length $ Array.filter isHpOrgan
        (Array.mapMaybe (getOrganAtPosition b) neighbors)
   in if hpOrgans > 0
      then HpClue hpOrgans
      else EmptyClue

hpCount :: Board -> Int
hpCount board = Array.length $ Array.filter isHpOrgan (intactOrgans board)

intactOrgans :: Board -> Array Organ
intactOrgans board = Tuple.fst <$> (getOrgans board).intact

getOrgans :: Board ->
  { intact :: Array InternalOrgan, injured :: Array InternalOrgan }
getOrgans (Board board) = {intact, injured}
  where
  noInjuries (Tuple (Organ (OrganSize w h) _) (V {x, y})) =
    let xmin = x
        xmax = w + x - 1
        ymin = y
        ymax = y + h - 1
    in not $ any (\(V i) ->
       i.x >= xmin
       && i.x <= xmax
       && i.y >= ymin
       && i.y <= ymax) board.injuries
  {yes: intact, no: injured} = Array.partition noInjuries (organArray board.organs)

data Clue =
  HpClue Int -- Health only
  | ArmorClue Int -- Armor only
  | MixedClue Int -- HealthAndArmor
  | ConcealedClue
  | EmptyClue

randomUninjuredSpace :: Board -> R.Random BoardCoord
randomUninjuredSpace (Board b) =
  let
    cart :: Array (Vector Int)
    cart = do
      x <- Array.range 0 5
      y <- Array.range 0 5
      pure $ V {x,y}
    uninjured :: Array (Vector Int)
    uninjured = Array.filter (\x -> not $ Set.member x b.injuries) cart
   in R.unsafeElement uninjured

newtype Health = Health
  { hpCount :: Int
  , board :: Board
  }

injure :: BoardCoord -> Health -> Health
injure v (Health h) =
  let board = injureBoard v h.board
   in Health { hpCount: hpCount board, board }

derive instance newtypeHealth :: Newtype Health _
