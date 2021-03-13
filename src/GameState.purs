module GameState where

import Extra.Prelude
import Framework.Direction (Direction, move)
import Framework.Direction as Direction
import Data.Array as Array
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Set as Set
import Data.Map as Map
import Data.Tuple as Tuple
import Data.LinearIndex (LinearIndex (..))
import Data.LinearIndex as LI
import Data.Foldable (minimum)
import Data.FoldableWithIndex (findWithIndex)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Position (Position (..))
import Data.Terrain
  (Terrain (..), demoTerrain, charToTerrain
  , flatten
  , arena
  , bareMap
  , carveRooms
  , Room
  )
import Data.Terrain as Terrain
import Solver as Solver
import Data.Ord (abs)
import Mapgen as G
import Data.Board
  ( BoardCoord
  , Board(..)
  , Organ(..)
  , InternalOrgan
  , OrganSize(..)
  , OrganType(..)
  , hpCount
  , OrganBag
  , emptyBag
  , insertOrgan
  , Health(..)
  , randomUninjuredSpace
  , canInsertOrgan
  , removeOrganAt
  , organExtent
  , isValidBoardCoord
  , extent
  , injure
  )
import Data.Enemy
  ( Enemy(..)
  , EnemyTag(..)
  , EnemyId
  , enemyOnSpace
  , injureEnemy
  , injureEnemyMulti
  )
import Random.Gen as R
import Random.Gen (Random)
import Data.Item
  ( Item(..)
  , ItemId
  , ItemTag (..)
  , medium
  )

newState :: Effect GameState
newState = do
  random <- R.newGen
  pure $ GameState
    { p: startingPos
    , playerHealth: freshPlayerHealth
    , playerDistanceMap: Map.empty
    , enemies: exampleEnemies
    , items: exampleItems
    , level: Surgery 1
    , availableOrgans: exampleOrgans
    , events: []
    , rng: random
    , terrain: bareMap
    , rooms: Map.empty
    }
    # genNewMap
    # revealRooms
    # recalculatePDMap

startingPos :: Vector Int
startingPos = V {x: 1, y: 1}

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

exampleItems :: Map ItemId Item
exampleItems =
  Map.fromFoldableWithIndex
  [
    Item
    { location: V{x: 10, y: 10}
    , decay: 10
    , tag: HealthPickup medium
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
  if String.length s' == arena.width*arena.height
    then Just $ LinearIndex {width: arena.width, height: arena.height, values: t}
    else Nothing
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
      -- # insertOrgan (vec 3 1) playerHpOrgan
      # insertOrgan (vec 2 3) playerHpOrgan
  , injuries: Set.empty
  }

hpOrgan1 :: Organ
hpOrgan1 = Organ (OrganSize 1 1) Hp

playerHpOrgan :: Organ
playerHpOrgan = Organ (OrganSize 2 2) PlayerHeartLarge

isWall :: Vector Int -> LinearIndex Terrain -> Boolean
isWall v t = (==) Wall $ fromMaybe Floor (LI.index t v)

isFloor :: Vector Int -> LinearIndex Terrain -> Boolean
isFloor v t = (==) Floor $ fromMaybe Floor (LI.index t v)

isOpenDoor :: Vector Int -> LinearIndex Terrain -> Boolean
isOpenDoor v t = (==) DoorOpen $ fromMaybe Floor (LI.index t v)

step :: GameState -> GameAction -> Either FailedAction GameState
step gs = handleAction (clearEvents <<< checkDeath $ gs)

checkDeath :: GameState -> GameState
checkDeath (GameState gs) =
  let
    (Health h) = gs.playerHealth
  in if h.hpCount <= 0
     then GameState gs {p = startingPos, playerHealth = freshPlayerHealth, enemies = exampleEnemies, level = Regular 1 }
     else GameState gs

handleAction :: GameState -> GameAction -> Either FailedAction GameState
handleAction g@(GameState gs) a@(Move dir) =
  let p' = move dir gs.p
   in case isPassable p' g, isExit p' g of
      false, _ -> Left (FailedAction dir)
      true,false -> Right $ (GameState gs {p = p'})
                      # reportEvent (PlayerMoved dir)
                      # revealRooms
                      # recalculatePDMap
                      # enemyTurn
      true,true -> Right $ (GameState gs {p = p'})
                      # goToNextLevel
                      # reportEvent (PlayerMoved dir)
                      # genNewMap
                      # recalculatePDMap

handleAction (GameState gs) a@(Attack bc eid) =
  let menemy = Map.lookup eid gs.enemies
   in case menemy of
    Nothing -> Left FailedAttack
    Just enemy -> Right $ handleEnemyInjury (GameState gs) enemy eid bc
        # reportEvent (PlayerAttacked eid)
        # enemyTurn
handleAction g a@(InstallOrgan organ bc) =
  if canInstallOrgan bc organ g
    then Right $ g
               # installOrgan bc (Tuple.fst organ)
               # removeAvailableOrgan organ
    else Left FailedInstall
handleAction g (RemoveOrgan p) = Right $ g # removeOrgan p
handleAction g FinishSurgery = Right $ goToNextLevel g
handleAction (GameState gs) _ = Right $ GameState gs

handleEnemyInjury :: GameState -> Enemy -> EnemyId -> BoardCoord -> GameState
handleEnemyInjury (GameState gs) (Enemy e) eid bc =
  let
    (Enemy newE) = injureEnemy bc (Enemy e)
    (Health h) = newE.health
  in if h.hpCount <= 0
     then GameState gs {enemies = Map.delete eid gs.enemies} # reportEvent (EnemyDied eid)
     else (GameState gs {enemies = Map.insert eid (Enemy newE) gs.enemies})

isPassable :: Vector Int -> GameState -> Boolean
isPassable t (GameState gs) =
  inWorldBounds t gs.terrain
  && not (isWall t gs.terrain)
  && not (any (enemyOnSpace t) gs.enemies)
  && t /= gs.p

isExit :: Vector Int -> GameState -> Boolean
isExit v (GameState gs) = case LI.index gs.terrain v of
  Just Exit -> true
  _ -> false

inWorldBounds :: Vector Int -> LinearIndex Terrain -> Boolean
inWorldBounds (V{x,y}) (LinearIndex t) =  -- TODO: fix this
  0 <= x && x <= t.width - 1 && 0 <= y && y <= t.height - 1

data Event =
    PlayerMoved Direction
  | EnemyMoved EnemyId (Vector Int)
  | PlayerAttacked EnemyId
  | EnemyAttacked EnemyId BoardCoord
  | PlayerDied
  | EnemyDied EnemyId
  | RoomRevealed Terrain.Room

newtype GameState = GameState
  { p :: Vector Int
  , playerHealth :: Health
  , playerDistanceMap :: Map (Vector Int) Int
  , enemies :: Map EnemyId Enemy
  , items :: Map ItemId Item
  , terrain :: LinearIndex Terrain
  , rooms :: Map Terrain.Room RoomInfo
  , level :: Level
  , availableOrgans :: OrganBag
  , events :: Array Event
  , rng :: R.Gen
  }

type RoomInfo =
  { room :: Terrain.Room
  , perimeter :: Terrain.Room
  , visible :: Boolean
  }

withRandom :: (GameState -> R.Random GameState) -> GameState -> GameState
withRandom f gs@(GameState g) =
  let {result: (GameState nextG), nextGen} = R.runRandom (f gs) g.rng
   in GameState nextG{ rng = nextGen }

genNewMap :: GameState -> GameState
genNewMap = withRandom $ \(GameState gs) -> do
  let {width,height} = arena
      conf = { width
             , height
             , minBlock: 3
             , maxBlock: 8
             }
  {rooms, entrance,exit,doors} <-  G.generateMapFull conf
  let terrain = bareMap
              # carveRooms rooms
              # Terrain.placeDoors doors
      roomInfo = rooms <#> \room ->
        { room, perimeter: Terrain.perimeter room, visible: false}
  pure $ GameState gs
    { terrain = terrain
    , rooms = Map.fromFoldable (Array.zip rooms roomInfo)
    }

recalculatePDMap :: GameState -> GameState
recalculatePDMap (GameState gs) =
  let start = gs.p
      expand x = do
         d <- Direction.directions8
         let x' = move d x
         guard $ inWorldBounds x' gs.terrain
         guard $ (isFloor x' gs.terrain || isOpenDoor x' gs.terrain)
         pure x'
      newMap = Solver.distanceMap start expand
   in GameState gs { playerDistanceMap = newMap }

revealRooms :: GameState -> GameState
revealRooms = withRandom \g@(GameState gs) ->
  -- random not used yet, but will be used when spawning things in
  let hiddenRooms = gs.rooms
                  # Map.filter (not <<< _.visible)
                  # Map.values
                  # Array.fromFoldable
      roomToReveal :: Maybe RoomInfo
      roomToReveal = hiddenRooms
                   # Array.find \r ->
                     Terrain.insideBlock gs.p r.perimeter
   in case roomToReveal of
        Nothing -> pure g
        Just r -> pure $
          GameState gs { rooms = Map.update
                                   (\x -> Just x{visible = true})
                                   r.room
                                   gs.rooms
                       }
          # reportEvent (RoomRevealed r.room)

spawnEnemies :: GameState -> Room -> Random GameState
spawnEnemies g@(GameState gs) room = pure g

spawnItems :: GameState -> Room -> Random GameState
spawnItems g@(GameState gs) room = pure g

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
getTerrainAtPosition p (GameState {terrain}) = fromMaybe Floor $ LI.index terrain p

type WeaponId = Int

data GameAction =
  Move Direction
  | Attack BoardCoord EnemyId
  | SelectWeapon WeaponId
  | InstallOrgan InternalOrgan BoardCoord
  | RemoveOrgan BoardCoord
  | FinishSurgery

data FailedAction =
  FailedAction Direction
  | FailedAttack
  | FailedInstall

enemyTurn :: GameState -> GameState
enemyTurn g@(GameState gs) = foldrWithIndex enemyAction g gs.enemies

enemyAction :: EnemyId -> Enemy -> GameState -> GameState
enemyAction eid (Enemy e) = withRandom \g@(GameState gs) -> do
  let candidates = Direction.directions8 <#> \dir ->
                   let target = move dir e.location in
                     { dir, target
                     , dist: fromMaybe 1000 $ Map.lookup target gs.playerDistanceMap
                     }
      minDist = fromMaybe 1000 $ minimum (candidates <#> _.dist)
      goodCandidates = Array.filter (\c -> c.dist == minDist) candidates
  {dir, target, dist} <- R.unsafeElement goodCandidates
  let newE = if isPassable target (GameState gs) && dist < 1000
               then Enemy e { location = target }
               else Enemy e
  pure $ case dist < 1000, target == gs.p of
       false,_ -> g
       true, true -> enemyAttack g eid
       true, false -> enemyMove g eid
         (Enemy e {location = target})
         (Direction.dirVector dir)

enemyMove :: GameState -> EnemyId -> Enemy -> Vector Int -> GameState
enemyMove (GameState gs) eid newE moveDir = GameState gs{ enemies = Map.insert eid newE gs.enemies }
      # reportEvent (EnemyMoved eid moveDir)

enemyAttack :: GameState -> EnemyId -> GameState
enemyAttack g eid = withRandom go g
  where
  go :: GameState -> R.Random GameState
  go (GameState gs) = do
    let (Health h) = gs.playerHealth
    attack <- randomUninjuredSpace h.board
    let (Health newHealth) = injure attack gs.playerHealth
    pure $ GameState gs { playerHealth = (Health newHealth) }
       # if newHealth.hpCount <= 0
         then reportEvent PlayerDied
         else reportEvent (EnemyAttacked eid attack)

canInstallOrgan :: Vector Int -> InternalOrgan -> GameState -> Boolean
canInstallOrgan pos organ (GameState g) =
 let bag = g.playerHealth
       # un Health
       # _.board
       # un Board
       # _.organs
     o = Tuple.fst organ
  in canInsertOrgan pos o bag && all isValidBoardCoord (extent o pos)

installOrgan :: BoardCoord -> Organ -> GameState -> GameState
installOrgan pos organ (GameState g) =
  let health = un Health g.playerHealth
      board = un Board health.board
      newBag = insertOrgan pos organ board.organs
      newBoard = board {organs = newBag}
   in GameState g{playerHealth = mkHealth (Board newBoard)}

removeAvailableOrgan :: InternalOrgan -> GameState -> GameState
removeAvailableOrgan organ (GameState gs) = GameState gs
  { availableOrgans = removeOrganAt (Tuple.snd organ) gs.availableOrgans }

removeOrgan :: BoardCoord -> GameState -> GameState
removeOrgan pos (GameState g) =
  let health = un Health g.playerHealth
      board = un Board health.board
      spacesToInjure = organExtent pos board.organs
      newBag = removeOrganAt pos board.organs
      newBoard = board {organs = newBag}
      newHealth = foldr injure (mkHealth (Board newBoard)) spacesToInjure
   in GameState g{playerHealth = mkHealth (Board newBoard)}

