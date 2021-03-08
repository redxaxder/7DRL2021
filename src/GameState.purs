module GameState where

import Extra.Prelude
import Framework.Direction (Direction, move, directions8)
import Data.Set as Set
import Data.Map as Map
import Data.Array as Array
import Data.LinearIndex (LinearIndex (..))
import Data.LinearIndex as LI
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Position (Position (..))
import Data.Tuple as Tuple
import Random.Gen as R

newState :: Effect GameState
newState = do
  random <- R.newGen
  pure $ GameState
   { p: V {x: 1, y:1}
   , playerHealth: freshPlayerHealth
   , enemies: Map.empty
   , terrain: fromMaybe (LI.fill 40 40 Floor) (freshTerrainFromString demoTerrain)
   , rng: random
   }

data Terrain = Wall | Floor | Exit
derive instance terrainEq :: Eq Terrain

charToTerrain :: Char -> Terrain
charToTerrain '.' = Floor
charToTerrain '>' = Exit
charToTerrain _ = Wall

freshTerrainFromString :: String -> Maybe (LinearIndex Terrain)
freshTerrainFromString s = if String.length s == 40*40 then Just $ LinearIndex {width: 40, height: 40, values: t} else Nothing
  where
    t = map charToTerrain $ toCharArray s

freshPlayerHealth :: Health
freshPlayerHealth = Health
  { hpCount: hpCount freshPlayerBoard
  , board: freshPlayerBoard
  }

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
    in any (\(BoardCoord (V i)) ->
       i.x >= xmin
       && i.x <= xmax
       && i.y >= ymin
       && i.y <= ymax) board.injuries

freshPlayerBoard :: Board
freshPlayerBoard = Board
  { organs: [ Tuple playerHpOrgan (BoardCoord (vec 2 2)) ]
  , injuries: Set.empty
  }

playerHpOrgan :: Organ
playerHpOrgan = Organ (OrganSize 2 2) Hp

isWall :: Vector Int -> LinearIndex Terrain -> Boolean
isWall v t = (==) Wall $ fromMaybe Floor (LI.index t (fromVector v))

fromVector :: Vector Int -> Position
fromVector (V v) = Position { x: v.x, y: v.y }

enemyOnSpace :: Vector Int -> Enemy -> Boolean
enemyOnSpace v (Enemy e) = e.location == v

step :: GameState -> GameAction -> Either FailedAction GameState
step (GameState gs) a@(Move dir) =
  let p' = move dir gs.p
   in if (inWorldBounds p' gs.terrain && not (isWall p' gs.terrain) && not (any (enemyOnSpace p') gs.enemies))
      then Right (GameState gs {p = p'})
      else Left (FailedAction dir)
step (GameState gs) _ = Right $ GameState gs

inWorldBounds :: Vector Int -> LinearIndex Terrain -> Boolean
inWorldBounds (V{x,y}) (LinearIndex t) =  -- TODO: fix this
  0 <= x && x <= t.width - 1 && 0 <= y && y <= t.height - 1

newtype GameState = GameState
  { p :: Vector Int
  , playerHealth :: Health
  , enemies :: Map EnemyId Enemy
  , terrain :: LinearIndex Terrain
  , rng :: R.Gen
  }

type EnemyId = Int
type WeaponId = Int

data GameAction =
  Move Direction
  | Attack BoardCoord EnemyId
  | SelectWeapon WeaponId
  | InstallOrgan Organ BoardCoord
  | RemoveOrgan Int

data FailedAction = FailedAction Direction

newtype Health = Health
  { hpCount :: Int
  , board :: Board
  }

derive instance newtypeHealth :: Newtype Health _

data OrganSize = OrganSize Int Int
data OrganType = Hp
data Organ = Organ OrganSize OrganType

newtype Enemy = Enemy
  { location :: Vector Int
  , health :: Health
  , clueCache :: Map BoardCoord Clue
  , tag :: EnemyTag
  }

data EnemyTag = Roomba


enemyName :: EnemyTag -> String
enemyName Roomba = "murderous vacuum robot"

newtype BoardCoord = BoardCoord (Vector Int)

derive instance eqBoardCoord :: Eq BoardCoord
derive instance ordBoardCoord :: Ord BoardCoord

newtype Board = Board
  { organs :: Array (Tuple Organ BoardCoord)
  -- , organIndex :: Map BoardCoord Organ
  , injuries :: Set BoardCoord
  }

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

data Clue =
  HpClue Int -- Health only
  | ArmorClue Int -- Armor only
  | MixedClue Int -- HealthAndArmor
  | ConcealedClue
  | EmptyClue

demoTerrain :: String
demoTerrain = """
########################################
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#......................................#
#..................................>...#
#......................................#
#......................................#
#......................................#
########################################
"""
