module GameState where

import Extra.Prelude
import Framework.Direction (Direction, move)
import Framework.Direction as Direction
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Set as Set
import Data.Map as Map
import Data.Tuple as Tuple
import Data.List as List
import Data.LinearIndex (LinearIndex (..))
import Data.LinearIndex as LI
import Data.Foldable (minimum, foldM)
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
import Data.Board as Board
import Data.Board
  ( BoardCoord
  , Board(..)
  , Organ(..)
  , InternalOrgan
  , OrganSize(..)
  , OrganType(..)
  , OrganBag
  , Health(..)
  , hpCount
  , emptyBag
  , insertOrgan
  , addOrgan
  , addOrgans
  , randomUninjuredSpace
  , canInsertOrgan
  , removeOrganAt
  , organExtent
  , isValidBoardCoord
  , extent
  , injure
  , injureMulti
  , freshHealth
  , randomInjuredSpace
  )
import Data.Enemy as Enemy
import Data.Enemy
  ( Enemy(..)
  , EnemyTag(..)
  , EnemyId
  , EnemyStats
  , enemyOnSpace
  , recalculateClues
  , injureEnemy
  , injureEnemyMulti
  )
import Random.Gen as R
import Random.Gen (Random)
import Data.Item
  ( Item(..)
  , ItemId
  , ItemTag (..)
  , Weapon
  , pistol
  , medium
  , itemOnSpace
  )

initState :: Effect GameState
initState = do
  random <- R.newGen
  pure $ GameState
    { p: startingPos
    , playerHealth: freshPlayerHealth
    , playerDistanceMap: Map.empty
    , playerWeapons: Map.singleton 0 pistol
    , playerCurrentWeapon: 0
    , enemies: Map.empty -- exampleEnemies
    , items: Map.empty -- exampleItems
    , level: NewGame
    , availableOrgans: exampleOrgans
    , events: []
    , rng: random
    , terrain: bareMap
    , rooms: Map.empty
    , nextId: 100
    }
    # genNewMap
    # revealRooms
    # recalculatePDMap

die :: GameState -> GameState
die (GameState gs) = (GameState gs
    { p = startingPos
    , playerHealth = freshPlayerHealth
    , playerDistanceMap = Map.empty
    , enemies = Map.empty -- exampleEnemies
    , items = Map.empty -- exampleItems
    , level = Dead
    , availableOrgans = exampleOrgans
    , events = []
    , rng = gs.rng
    , terrain = bareMap
    , rooms = Map.empty
    , nextId = gs.nextId
    })
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
    , health: Board.fromBoard exampleRoombaBoard
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
freshPlayerHealth = Board.fromBoard freshPlayerBoard

freshPlayerBoard :: Board
freshPlayerBoard = Board
  { organs: emptyBag
      # insertOrgan (vec 1 1) humanEye
      # insertOrgan (vec 0 3) playerHpOrgan
      # insertOrgan (vec 5 4) humanEye
      # insertOrgan (vec 3 0) playerHpOrgan
      # insertOrgan (vec 3 3) playerHpOrgan
  , injuries: Set.empty
  }

hpOrgan1 :: Organ
hpOrgan1 = Organ (OrganSize 1 1) Hp

playerHpOrgan :: Organ
playerHpOrgan = Organ (OrganSize 2 2) PlayerHeartLarge

humanEye :: Organ
humanEye = Organ (OrganSize 1 1) HumanEye

eyeRed :: Organ
eyeRed = Organ (OrganSize 1 1) EyeRed

eyeBlue :: Organ
eyeBlue = Organ (OrganSize 1 1) EyeBlue

eyeH :: Organ
eyeH = Organ (OrganSize 1 1) EyeHoriz

eyeV :: Organ
eyeV = Organ (OrganSize 1 1) EyeVert

isWall :: Vector Int -> LinearIndex Terrain -> Boolean
isWall v t = (==) Wall $ fromMaybe Floor (LI.index t v)

isFloor :: Vector Int -> LinearIndex Terrain -> Boolean
isFloor v t = (==) Floor $ fromMaybe Floor (LI.index t v)

isOpenDoor :: Vector Int -> LinearIndex Terrain -> Boolean
isOpenDoor v t = (==) DoorOpen $ fromMaybe Floor (LI.index t v)

step :: GameState -> GameAction -> Either FailedAction GameState
step gs = handleAction (clearEvents <<< checkDeath <<< cleanupItems <<< decayItems $ gs)

checkDeath :: GameState -> GameState
checkDeath (GameState gs) =
  let
    (Health h) = gs.playerHealth
  in if h.hpCount <= 0
     then die (GameState gs)
     else GameState gs

handleAction :: GameState -> GameAction -> Either FailedAction GameState
handleAction g@(GameState gs) a@(Move dir) =
  let
    p' = move dir gs.p
   in case isPassable p' g, isExit p' g, getItem p' g of
      false, _, _ -> Left (FailedAction dir)
      true,false,Nothing -> Right $ (GameState gs {p = p'})
        # reportEvent (PlayerMoved dir)
        # revealRooms
        # recalculatePDMap
        # enemyTurn
        # openDoorAt p'
      true,false,Just{item,iid} -> Right $ (GameState gs {p = p'}) 
        # withRandom (healMany item)
        # useItem iid
        # reportEvent (PlayerMoved dir)
        # reportEvent (ItemUsed item)
        # revealRooms
        # recalculatePDMap
        # enemyTurn
        # openDoorAt p'
      true,true,_ -> Right $ (GameState gs {p = p'})
        # goToNextLevel
        # reportEvent (PlayerMoved dir)
        # genNewMap
        # genNewOrgans
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
handleAction g StartNewGame = Right $ goToNextLevel g
handleAction (GameState gs) _ = Right $ GameState gs

handleEnemyInjury :: GameState -> Enemy -> EnemyId -> BoardCoord -> GameState
handleEnemyInjury (GameState gs) (Enemy e) eid bc =
  let newE = injureEnemy bc (Enemy e)
  in if Enemy.isAlive newE
     then (GameState gs {enemies = Map.insert eid newE gs.enemies})
     else GameState gs {enemies = Map.delete eid gs.enemies}
          # reportEvent (EnemyDied eid)

isPassable :: Vector Int -> GameState -> Boolean
isPassable t (GameState gs) =
  inWorldBounds t gs.terrain
  && not (isWall t gs.terrain)
  && not (any (enemyOnSpace t) gs.enemies)

isExit :: Vector Int -> GameState -> Boolean
isExit v (GameState gs) = case LI.index gs.terrain v of
  Just Exit -> true
  _ -> false

isItem :: Vector Int -> GameState -> Boolean
isItem v (GameState gs) = any (itemOnSpace v) gs.items

getItem :: Vector Int -> GameState -> Maybe { item :: Item, iid :: ItemId }
getItem v (GameState gs) = do
  let newM = Map.filter (\(Item i) -> i.location == v) gs.items
  item <- List.head (Map.values newM)
  iid <- Array.head $ Set.toUnfoldable (Map.keys newM)
  pure { item, iid }

inWorldBounds :: Vector Int -> LinearIndex Terrain -> Boolean
inWorldBounds (V{x,y}) (LinearIndex t) =
  0 <= x && x <= t.width - 1 && 0 <= y && y <= t.height - 1

useItem :: ItemId -> GameState -> GameState
useItem iid (GameState gs) = GameState gs { items = Map.delete iid gs.items }

decayItems :: GameState -> GameState
decayItems (GameState gs) =
  let newItems = map ( \(Item i) -> (Item i { decay = i.decay - 1 }) ) gs.items
  in (GameState gs { items = newItems })

cleanupItems :: GameState -> GameState
cleanupItems (GameState gs) = GameState gs { items = Map.filter (\(Item i) -> i.decay > 0) gs.items }

data Event =
    PlayerMoved Direction
  | EnemyMoved EnemyId (Vector Int)
  | PlayerAttacked EnemyId
  | EnemyAttacked EnemyId BoardCoord
  | PlayerDied
  | EnemyDied EnemyId
  | RoomRevealed Terrain.Room
  | ItemUsed Item

newtype GameState = GameState
  { p :: Vector Int
  , playerHealth :: Health
  , playerDistanceMap :: Map (Vector Int) Int
  , playerWeapons :: Map Int Weapon
  , playerCurrentWeapon :: Int
  , enemies :: Map EnemyId Enemy
  , items :: Map ItemId Item
  , terrain :: LinearIndex Terrain
  , rooms :: Map Terrain.Room RoomInfo
  , level :: Level
  , availableOrgans :: OrganBag
  , events :: Array Event
  , rng :: R.Gen
  , nextId :: Int
  }

type RoomInfo =
  { room :: Terrain.Room
  , perimeter :: Terrain.Room
  , visible :: Boolean
  }

openDoorAt :: Vector Int -> GameState -> GameState
openDoorAt p (GameState gs) =
  let newTerrain = fromMaybe gs.terrain do
                      t <-  LI.index gs.terrain p
                      guard $ t == DoorClosed
                      LI.insertAt p DoorOpen gs.terrain
   in GameState gs{terrain = newTerrain}

withRandom :: (GameState -> R.Random GameState) -> GameState -> GameState
withRandom f gs@(GameState g) =
  let {result: (GameState nextG), nextGen} = R.runRandom (f gs) g.rng
   in GameState nextG{ rng = nextGen }

--------------------------------------------------------------------------------
-- Gen Stuff -------------------------------------------------------------------
--------------------------------------------------------------------------------

genNewMap :: GameState -> GameState
genNewMap = withRandom $ \(GameState gs) -> do
  let {width,height} = arena
      conf = { width
             , height
             , minBlock: 3
             , maxBlock: 8
             }
  { rooms, entrance,exit,doors } <-  G.generateMapFull conf
  let terrain = bareMap
              # carveRooms rooms
              # Terrain.placeDoors doors
              # Terrain.placeExit exit
      roomInfo = rooms <#> \room ->
        { room, perimeter: Terrain.perimeter room, visible: false}
  pure $ GameState gs
    { terrain = terrain
    , rooms = Map.fromFoldable (Array.zip rooms roomInfo)
    }

genNewOrgans :: GameState -> GameState
genNewOrgans gs = gs -- TODO

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

populateRoom :: GameState -> Room -> Random GameState
populateRoom g room = do
  let weight 0 = 4
      weight 1 = 4
      weight _ = 2
  numberOfEnemies <- R.unsafeWeightedElement "populateRoom" weight [0,1,2]
  {head, tail} <- (unsafeFromJust <<< Array.uncons) <$>
                     rollLocations (1 + numberOfEnemies) room
  item <- genItem head g
  enemies <- for tail \p -> genEnemy p g
  pure $ g
    # addItem item
    # flip (foldr addEnemy) (Array.filter Enemy.isAlive enemies)

rollLocations :: Int -> {x::Int,y::Int,width::Int,height::Int}
    -> Random (Array BoardCoord)
rollLocations 0 _ = pure []
rollLocations n room = do
  let candidates = Terrain.blockPositions room
  for (Array.range 1 n) \_ -> R.unsafeElement "rollLocations" candidates

genItem :: Vector Int -> GameState -> Random Item
genItem location (GameState gs) = do
  let d = levelDepth gs.level -- todo: incorporate this
  pure $ Item {location, decay: 15, tag: HealthPickup 5}

genEnemy :: Vector Int -> GameState -> Random Enemy
genEnemy location (GameState gs)= do
  let d = levelDepth gs.level
      candidates = Enemy.allEnemies
                 # Array.filter (\x -> (Enemy.stats x).minDepth <= d)
  tag <- R.unsafeElement "genEnemy" candidates
  health <- genHealth (Enemy.stats tag)
  pure $ Enemy {location, health, tag, clueCache: Map.empty }
       # recalculateClues

genHealth :: EnemyStats -> Random Health
genHealth {armor, hp, injuries} = do
  let bounds = {x:0,y:0,width:6,height:6}
      armorOrgan = Organ (OrganSize 1 1) Armor
      healthOrgan = hpOrgan1
  as <- rollLocations armor bounds
  hs <- rollLocations hp bounds
  is <- rollLocations injuries bounds
  pure $ freshHealth
       # addOrgans as armorOrgan
       # addOrgans hs healthOrgan
       # injureMulti is

--------------------------------------------------------------------------------

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
        Just r -> do
          g' <- populateRoom g r.room
          pure $ g'
             # reportEvent (RoomRevealed r.room)
             # markVisible r.room

markVisible :: Room -> GameState -> GameState
markVisible room (GameState gs) =
  GameState gs { rooms = Map.update
                           (\x -> Just x{visible = true})
                           room
                           gs.rooms
               }


addItem :: Item -> GameState -> GameState
addItem i (GameState gs) = GameState gs
  { items = Map.insert gs.nextId i gs.items
  , nextId = gs.nextId + 1
  }


addEnemy :: Enemy -> GameState -> GameState
addEnemy e (GameState gs) = GameState gs
  { enemies = Map.insert gs.nextId e gs.enemies
  , nextId = gs.nextId + 1
  }

data Level = Regular Int | Surgery Int | NewGame | Dead | Victory

isSurgeryLevel :: GameState -> Boolean
isSurgeryLevel (GameState {level: Surgery _}) = true
isSurgeryLevel _ = false

nextLevel :: Level -> Level
nextLevel (Regular i) = if (spy "level i" i) < 5 then Surgery i else Victory
nextLevel (Surgery i) = Regular (i+1)
nextLevel NewGame = Regular 1
nextLevel Dead = NewGame
nextLevel Victory = Victory

levelDepth :: Level -> Int
levelDepth (Regular i) = i
levelDepth (Surgery i) = i
levelDepth NewGame = 0
levelDepth Victory = 9001
levelDepth Dead = 0

goToNextLevel :: GameState -> GameState
goToNextLevel (GameState gs) = GameState gs { level = nextLevel gs.level }

clearEvents :: GameState -> GameState
clearEvents (GameState gs) = GameState gs{events = []}

reportEvent :: Event -> GameState -> GameState
reportEvent e (GameState gs) = GameState gs
  { events = Array.cons e gs.events }

data Target =
  TargetEnemy EnemyId
  | TargetItem ItemId
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
  | StartNewGame

data FailedAction =
  FailedAction Direction
  | FailedAttack
  | FailedInstall

--------------------------------------------------------------------------------
--- Enemies --------------------------------------------------------------------
--------------------------------------------------------------------------------

enemyTurn :: GameState -> GameState
enemyTurn g@(GameState gs) = foldrWithIndex enemyAction g gs.enemies

enemyAction :: EnemyId -> Enemy -> GameState -> GameState
enemyAction eid (Enemy e) = withRandom \g@(GameState gs) -> do
  let candidates = Direction.directions8 <#> \dir ->
                   let target = move dir e.location in
                     { dir, target
                     , dist: fromMaybe 1000 $ Map.lookup target gs.playerDistanceMap
                     }
      isFree {target:t} =  isNothing (getEnemyAtPosition t g) && isPassable t g
      freeSpaces = Array.filter isFree candidates
      minDist = fromMaybe 1000 $ minimum (freeSpaces <#> _.dist)
      goodCandidates = Array.filter (\c -> c.dist == minDist) freeSpaces
  case NonEmpty.fromArray goodCandidates of
       Nothing -> pure g
       Just c -> do
         {dir, target, dist} <- R.element c
         let newE = if dist < 1000
               then Enemy e { location = target }
               else Enemy e
         pure $ case dist < 1000, target == gs.p of
               false,_ -> g
               true, true -> enemyAttack g eid
               true, false -> enemyMove g eid newE
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
        canAttack = Just DoorClosed /= LI.index gs.terrain gs.p
    mattack <- randomUninjuredSpace h.board
    case canAttack, mattack of
         true, Just attack -> do
           let newHealth = injure attack gs.playerHealth
           pure $ GameState gs { playerHealth = newHealth }
                # if Board.isAlive newHealth
                  then reportEvent (EnemyAttacked eid attack)
                  else reportEvent PlayerDied
         _,_ -> pure g

--------------------------------------------------------------------------------
--- Organs ---------------------------------------------------------------------
--------------------------------------------------------------------------------

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
   in GameState g{playerHealth = Board.fromBoard (Board newBoard)}

removeAvailableOrgan :: InternalOrgan -> GameState -> GameState
removeAvailableOrgan organ (GameState gs) = GameState gs
  { availableOrgans = removeOrganAt (Tuple.snd organ) gs.availableOrgans }

removeOrgan :: BoardCoord -> GameState -> GameState
removeOrgan pos (GameState g) =
  let health = un Health g.playerHealth
      board = un Board health.board
      spacesToInjure = organExtent pos board.organs
      newBag = removeOrganAt pos board.organs
      newBoard = Board board {organs = newBag}
      newHealth = foldr injure (Board.fromBoard newBoard) spacesToInjure
   in GameState g{ playerHealth = newHealth }

healOne :: GameState -> R.Random GameState
healOne gs@(GameState g) =
  let
    (Health h) = g.playerHealth
    (Board b) = h.board
    hp = h.hpCount
  in do
    mbc <- randomInjuredSpace (Board b)
    case mbc of
         Nothing -> pure gs
         Just bc -> do
            let newI = Set.delete bc b.injuries
            let newB = Board b { injuries = newI }
            let newH = Health h { hpCount = hpCount newB, board = newB}
            pure $ GameState g { playerHealth = newH }

healMany :: Item -> GameState -> R.Random GameState
healMany (Item i) (GameState gs) =
  let
    tag = i.tag
    (Health h) = gs.playerHealth
    (Board b) = h.board
  in if (Set.size b.injuries) > 0
     then case tag of
       HealthPickup n -> do
         let nHeal = min n $ Set.size b.injuries
         newGs <- foldM (\g _ -> healOne g) (GameState gs) $ Array.range 1 n
         pure newGs
       _ -> pure $ GameState gs
     else pure $ GameState gs

--------------------------------------------------------------------------------
-- Weapon Management -----------------------------------------------------------
--------------------------------------------------------------------------------

getFreeWeaponSlot :: Map Int Weapon -> Maybe Int
getFreeWeaponSlot m = Array.find (\x -> not $ Map.member x m) [0,1,2]

placeWeapon :: Vector Int -> Weapon -> GameState -> GameState
placeWeapon p w = todo

pickUpWeapon :: ItemId -> GameState -> GameState
pickUpWeapon iid (GameState gs) = todo

selectWeapon :: Int -> GameState -> GameState
selectWeapon i gs = todo


--------------------------------------------------------------------------------
