module GameState where

import Extra.Prelude
import Framework.Direction (Direction, move)
import Data.Array as Array
import Data.Set as Set
import Data.Map as Map
import Data.Tuple as Tuple
import Data.LinearIndex (LinearIndex (..))
import Data.LinearIndex as LI
import Data.RevMap (RevMap)
import Data.RevMap as RevMap
import Data.FoldableWithIndex (findWithIndex)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Position (Position (..))
import Data.Terrain (Terrain (..), demoTerrain, charToTerrain, flatten)
import Data.Ord (abs)
import Data.Board
  ( BoardCoord(..)
  , Board(..)
  , Clue
  , Organ(..)
  , OrganSize(..)
  , OrganType(..)
  , hpCount
  , injureBoard
  , getClue
  , OrganBag
  , emptyBag
  , insertOrgan
  )
import Random.Gen as R

newState :: Effect GameState
newState = do
  random <- R.newGen
  pure $ GameState
   { p: V {x: 1, y:1}
   , playerHealth: freshPlayerHealth
   , enemies: exampleEnemies
   , level: Regular 1
   , availableOrgans: exampleOrgans
   , events: []
   , terrain: fromMaybe (LI.fill 40 40 Floor) (freshTerrainFromString demoTerrain)
   , rng: random
   }

exampleEnemies :: Map EnemyId Enemy
exampleEnemies =
  Map.fromFoldableWithIndex
  [ injureEnemyMulti exampleInjuries $ Enemy
    { location: V{x:5, y:5}
    , health: mkHealth exampleRoombaBoard
    , clueCache: Map.empty
    , tag: Roomba
    }
  ]

mkHealth :: Board -> Health
mkHealth board = Health
  { hpCount: hpCount board
  , board
  }

exampleInjuries :: Array BoardCoord
exampleInjuries = [ vec 1 1, vec 2 3, vec 4 4 ]

exampleRoombaBoard :: Board
exampleRoombaBoard = Board
  { organs: emptyBag
     # insertOrgan (vec 2 2) hpOrgan1
     # insertOrgan (vec 2 5) hpOrgan1
     # insertOrgan (vec 4 0) hpOrgan1
  , injuries: Set.empty
  }

exampleOrgans :: OrganBag
exampleOrgans = emptyBag
  # insertOrgan (vec 4 4) playerHpOrgan
  # insertOrgan (vec 6 8) playerHpOrgan

freshTerrainFromString :: String -> Maybe (LinearIndex Terrain)
freshTerrainFromString s =
  if String.length s' == 40*40 then Just $ LinearIndex {width: 40, height: 40, values: t} else Nothing
  where
    t = map charToTerrain $ toCharArray s'
    s' = flatten s

freshPlayerHealth :: Health
freshPlayerHealth = Health
  { hpCount: hpCount freshPlayerBoard
  , board: freshPlayerBoard
  }

freshPlayerBoard :: Board
freshPlayerBoard = Board
  { organs: emptyBag
      # insertOrgan (vec 1 1) playerHpOrgan
      # insertOrgan (vec 3 1) playerHpOrgan
      # insertOrgan (vec 2 3) playerHpOrgan
  , injuries: Set.empty
  }

hpOrgan1 :: Organ
hpOrgan1 = Organ (OrganSize 1 1) Hp

playerHpOrgan :: Organ
playerHpOrgan = Organ (OrganSize 2 2) PlayerHeartLarge

isWall :: Vector Int -> LinearIndex Terrain -> Boolean
isWall v t = (==) Wall $ fromMaybe Floor (LI.index t (fromVector v))

fromVector :: Vector Int -> Position
fromVector (V v) = Position { x: v.x, y: v.y }

enemyOnSpace :: Vector Int -> Enemy -> Boolean
enemyOnSpace v (Enemy e) = e.location == v

step :: GameState -> GameAction -> Either FailedAction GameState
step gs = handleAction (clearEvents gs)

handleAction :: GameState -> GameAction -> Either FailedAction GameState
handleAction g@(GameState gs) a@(Move dir) =
  let p' = move dir gs.p
   in case isPassable p' g, isExit p' g of
      false, _ -> Left (FailedAction dir)
      true,false -> Right $ (GameState gs {p = p'})
                      # reportEvent (PlayerMoved dir)
                      # enemyTurn
      true,true -> Right $ (GameState gs {p = p'})
                      # goToNextLevel
                      # reportEvent (PlayerMoved dir)

handleAction (GameState gs) a@(Attack bc eid) =
  let menemy = Map.lookup eid gs.enemies
   in case menemy of
    Nothing -> Left FailedAttack
    Just enemy -> Right $
      (GameState gs {enemies = Map.insert eid (injureEnemy bc enemy) gs.enemies})
        # reportEvent (PlayerAttacked eid)
        # enemyTurn
handleAction (GameState gs) _ = Right $ GameState gs

injureEnemy :: BoardCoord -> Enemy -> Enemy
injureEnemy bc (Enemy e) =
  let health@(Health {board}) = injure bc e.health
      clue = getClue bc board
   in Enemy e { health = health, clueCache = Map.insert bc clue e.clueCache}

injureEnemyMulti :: Array BoardCoord -> Enemy -> Enemy
injureEnemyMulti bcs e = foldr injureEnemy e bcs

injure :: BoardCoord -> Health -> Health
injure v (Health h) =
  let board = injureBoard v h.board
   in Health { hpCount: hpCount board, board }

isPassable :: Vector Int -> GameState -> Boolean
isPassable t (GameState gs) =
  inWorldBounds t gs.terrain
  && not (isWall t gs.terrain)
  && not (any (enemyOnSpace t) gs.enemies)
  && t /= gs.p

isExit :: Vector Int -> GameState -> Boolean
isExit v (GameState gs) = case LI.index gs.terrain (fromVector v) of
  Just Exit -> true
  _ -> false

enemyTurn :: GameState -> GameState
enemyTurn g@(GameState gs) = foldrWithIndex enemyAction g gs.enemies

enemyAction :: EnemyId -> Enemy -> GameState -> GameState
enemyAction eid (Enemy e) (GameState gs) =
  let target = e.location + (intVecToDir $ gs.p - e.location)
      moveDir = intVecToDir $ gs.p - e.location
      newE = if isPassable target (GameState gs)
               then Enemy e { location = e.location + moveDir }
               else Enemy e
   in if target == gs.p
      then enemyAttack (GameState gs) eid
      else enemyMove (GameState gs) eid newE moveDir

enemyMove :: GameState -> EnemyId -> Enemy -> Vector Int -> GameState
enemyMove (GameState gs) eid newE moveDir = GameState gs{ enemies = Map.insert eid newE gs.enemies }
      # reportEvent (EnemyMoved eid moveDir)

enemyAttack :: GameState -> EnemyId -> GameState
enemyAttack (GameState gs) eid =
  let
    (Health h) = gs.playerHealth
    attack = randomSpace h.board (GameState gs)
    newHealth = injure (Tuple.fst attack) gs.playerHealth
  in GameState gs { playerHealth = newHealth, rng = Tuple.snd attack }
     # reportEvent (EnemyAttacked eid $ Tuple.fst attack)

randomSpace :: Board -> GameState -> Tuple BoardCoord R.Gen
randomSpace (Board b) (GameState gs) =
  let 
    xRand :: { result :: Int, nextGen :: R.Gen }
    xRand = R.runRandom (R.intRange 0 7) gs.rng
    yRand :: { result :: Int, nextGen :: R.Gen }
    yRand = R.runRandom (R.intRange 0 7) xRand.nextGen
    attackCoord = V { x: xRand.result, y: yRand.result }
  in Tuple attackCoord yRand.nextGen

intVecToDir :: Vector Int -> Vector Int
intVecToDir (V {x,y}) = V {x: abs' x, y: abs' y}
  where abs' n = if n == 0 then 0 else n / abs n

inWorldBounds :: Vector Int -> LinearIndex Terrain -> Boolean
inWorldBounds (V{x,y}) (LinearIndex t) =  -- TODO: fix this
  0 <= x && x <= t.width - 1 && 0 <= y && y <= t.height - 1

data Event =
    PlayerMoved Direction
  | EnemyMoved EnemyId (Vector Int)
  | PlayerAttacked EnemyId
  | EnemyAttacked EnemyId BoardCoord

newtype GameState = GameState
  { p :: Vector Int
  , playerHealth :: Health
  , enemies :: Map EnemyId Enemy
  , terrain :: LinearIndex Terrain
  , level :: Level
  , availableOrgans :: OrganBag
  , events :: Array Event
  , rng :: R.Gen
  }

data Level = Regular Int | Surgery Int

isSurgeryLevel :: GameState -> Boolean
isSurgeryLevel (GameState {level: Surgery _}) = true
isSurgeryLevel _ = false

nextLevel :: Level -> Level
nextLevel (Regular i) = Surgery i
nextLevel (Surgery i) = Regular (i+1)

goToNextLevel :: GameState -> GameState
goToNextLevel (GameState gs) = GameState gs { level = nextLevel gs.level }

clearEvents :: GameState -> GameState
clearEvents (GameState gs) = GameState gs{events = []}

reportEvent :: Event -> GameState -> GameState
reportEvent e (GameState gs) = GameState gs
  { events = Array.cons e gs.events }

data Target =
  TargetEnemy EnemyId
  | TargetTerrain Terrain

getTargetAtPosition :: Vector Int -> GameState -> Target
getTargetAtPosition p gs = case getEnemyAtPosition p gs of
  Just eid -> TargetEnemy eid
  Nothing -> TargetTerrain $ getTerrainAtPosition p gs

getEnemyAtPosition :: Vector Int -> GameState -> Maybe EnemyId
getEnemyAtPosition p (GameState gs) = do
  -- loop through all enemies and check if one of them is at position p
  -- if the slowness ends up mattering, we should maintain an index instead
  {index} <- findWithIndex (\eid (Enemy e) -> e.location == p) gs.enemies
  pure index

getTerrainAtPosition :: Vector Int -> GameState -> Terrain
getTerrainAtPosition p (GameState {terrain}) = fromMaybe Floor $ LI.index terrain (fromVector p)

type EnemyId = Int
type WeaponId = Int

data GameAction =
  Move Direction
  | Attack BoardCoord EnemyId
  | SelectWeapon WeaponId
  | InstallOrgan Organ BoardCoord
  | RemoveOrgan Int
  | FinishSurgery

data FailedAction =
  FailedAction Direction
  | FailedAttack

newtype Health = Health
  { hpCount :: Int
  , board :: Board
  }

derive instance newtypeHealth :: Newtype Health _

newtype Enemy = Enemy
  { location :: Vector Int
  , health :: Health
  , clueCache :: Map BoardCoord Clue
  , tag :: EnemyTag
  }

data EnemyTag = Roomba

enemyName :: EnemyTag -> String
enemyName Roomba = "murderous vacuum robot"
