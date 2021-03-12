module UI where

import Extra.Prelude

import Data.Either (either)
import Data.Array as Array
import Data.Map as Map
import Data.Int as Int
import Data.DateTime.Instant (instant, unInstant)
import Data.Time.Duration (Milliseconds (..))
import Control.Alt ((<|>))

import Framework.Direction
  (down
  , left
  , right
  , up
  , move
  , Direction
  , dirVector
  , opposite
  )
import Framework.UI as F
import Framework.Audio as FA
import Framework.Render.Core (Rectangle)

import Animation (Animating, DiffTime(..))
import Animation as A
import Data.Board
  ( InternalOrgan
  , Board (..)
  , extent
  , Health (..)
  , organAt
  , canInsertOrgan
  , isValidBoardCoord
  )
import Data.Terrain (Terrain)
import Data.Tuple as Tuple
import GameState
  ( GameState (..)
  , GameAction (..)
  , FailedAction (..)
  , getTargetAtPosition
  , isSurgeryLevel
  , Target (..)
  , Event (..)
  )
import Data.Enemy (EnemyId)

import Input (Input, InputValue (..))

type UI r =
  F.UIM GameState GameAction FailedAction UIState Input r

newtype UIState = UIState
  { rightPaneTarget :: RightPane
  , lockedTarget :: RightPane
  , timestamp :: Instant
  , gsTimestamp :: Instant
  , playerAnim :: Offset
  , enemyAnim :: Map EnemyId Offset
  , highlights :: Array Rectangle
  , audioData :: AudioData
  , audioQueue :: AudioQueue
  , draggingOrgan :: Maybe OrganDrag
  }

type OrganDrag =
  { organ :: InternalOrgan
  , offset :: Vector Number
  }

data RightPane =
  RPEnemy EnemyId
  | RPTerrain Terrain
  | RPNoTarget

derive instance eqrp :: Eq RightPane

{-
data PointerState = Neutral
  | Dragging
    { pointerId :: Number
    , start :: Vector Number
    , current :: Vector Number
    }
  -}

initUIState :: GameState -> Effect UIState
initUIState (GameState {p}) = do
  audioData <- loadAudioData audioPaths
  pure $ UIState
    { rightPaneTarget: RPNoTarget
    , lockedTarget: RPNoTarget
    , timestamp: unsafeFromJust $ instant $ Milliseconds 0.0
    , gsTimestamp: unsafeFromJust $ instant $ Milliseconds 0.0
    , playerAnim: pure zero
    , draggingOrgan: Nothing
    , enemyAnim: Map.empty
    , audioData
    , audioQueue: []
    , highlights: []
    }

runUI :: UIState -> GameState -> UI Unit
runUI uis gs =
  let u = clearHighlights uis in
  case isSurgeryLevel gs of
  true -> surgeryUI u gs
  false -> mapUI u gs

mapUI :: UIState -> GameState -> UI Unit
mapUI uis@(UIState u) gs@(GameState g) = do
  { time, value } <- F.input uis
  case value of
       KeyDown key -> case getDir key of
           Nothing -> runUI uis gs
           Just d -> doAction (Move d) time uis gs
       PointerDown {location} ->
         case locate location of
              CenterPane p ->
                let newUI = lockTarget time p uis gs
                 in runUI newUI gs
              TargetBoard p -> tryAttack time p uis gs
              _ -> runUI uis gs
       PointerMove {location} ->
         case locate location of
              CenterPane p ->
                let newUI = viewTarget time p uis gs
                 in runUI newUI gs -- yes
              _otherLocations -> if u.lockedTarget /= u.rightPaneTarget
                           then let newUI = toLockedTarget time uis
                                 in runUI newUI gs
                                    else runUI uis gs
       _otherEvents -> runUI uis gs

--------------------------------------------------------------------------------
-- Surgery ---------------------------------------------------------------------
--------------------------------------------------------------------------------

surgeryUI :: UIState -> GameState -> UI Unit
surgeryUI uis gs@(GameState g) = do
  { time, value } <- F.input uis
  case value of
    PointerDown {location, pointerId} ->
      case locate location of
           CenterPane p ->
             -- check gamestate's organ map to see if an organ has been clicked
             case organAt p g.availableOrgans of
                  Nothing -> runUI uis gs
                  Just organ -> do
                    -- if so, enter drag mode holding that organ
                    {time: t, v: dragOffset} <-
                        dragOrgan time pointerId organ location uis gs
                    let newUIS = setDirtyAll t uis
                    --we've finished dragging, but where did we end up?
                    case locate (location + dragOffset) of
                         PlayerBoard pb ->
                           tryInstallOrgan t pb organ newUIS gs
                         _ -> runUI newUIS gs
           PlayerBoard p ->
             let b = (g.playerHealth # un Health # _.board # un Board # _.organs)
              in
                   case organAt p b of
                        Nothing -> runUI uis gs
                        Just (Tuple _ pos) ->
                          audioAction "Organ3.mp3" (RemoveOrgan pos) time
                            (setDirtyUI time uis) gs
           _ -> runUI uis gs
    _otherEvents -> runUI uis gs

dragOrgan
  :: Instant
  -> Number
  -> InternalOrgan
  -> Vector Number
  -> UIState
  -> GameState
  -> UI {time :: Instant, v :: Vector Number}
dragOrgan t ptrId organ initialClickPos uis gs = uis
  # setDirty
  # setOffset zero
  # enqueueAudio t "Puip1.mp3"
  # go
  where
  setDirty (UIState u) = UIState u{gsTimestamp = t}
  setOffset offset (UIState u) = UIState u{draggingOrgan =
                     Just { organ, offset }
                   }
  stop (UIState u) = UIState u{draggingOrgan = Nothing}
  go u =  do
    { time, value } <- F.input u
    let nextU = clearHighlights u
    case value of
         PointerMove {pointerId, location} ->
             let offset = location - initialClickPos
                 (Tuple o pos) = organ
                 newUIS = fromMaybe nextU $ do
                         l <- organDragLoc pos offset
                         guard (canInstallOrgan l organ gs)
                         pure $ nextU # highlights
                                (PlayerBoard <$> extent o l)
              in go (setOffset offset newUIS)

         PointerUp {pointerId, location} ->
           if pointerId == ptrId
             then pure $ {time, v: location - initialClickPos}
             else pure $ {time, v: zero}
         _ -> go nextU

organDragLoc :: Vector Int -> Vector Number -> Maybe (Vector Int)
organDragLoc startPos offset =
  let start = fromGrid startPos + V{x:tileSize / 2.0,y:tileSize / 2.0} + rectPos centerPaneRect
      end = start + offset
   in case locate end of
      PlayerBoard pb -> Just pb
      _ -> Nothing

--------------------------------------------------------------------------------
-- Target Management -----------------------------------------------------------
--------------------------------------------------------------------------------

rpTarget :: Vector Int -> GameState -> RightPane
rpTarget pos gs = case getTargetAtPosition pos gs of
  TargetEnemy eid -> RPEnemy eid
  TargetTerrain terrain -> RPTerrain terrain

viewTarget :: Instant -> Vector Int -> UIState -> GameState -> UIState
viewTarget t pos uis@(UIState u) gs =
  case rpTarget pos gs of
    target@(RPEnemy _) ->
        UIState u
        { rightPaneTarget = target
        , timestamp = t
        }
    _ -> toLockedTarget t uis

lockTarget :: Instant -> Vector Int -> UIState -> GameState -> UIState
lockTarget t pos (UIState u) gs =
  let target = rpTarget pos gs
   in UIState u
        { rightPaneTarget = target
        , lockedTarget = target
        , timestamp = t
        }

toLockedTarget :: Instant -> UIState -> UIState
toLockedTarget t (UIState u) = UIState u
  { rightPaneTarget = u.lockedTarget
  , timestamp = t
  }

getTarget :: UIState -> Maybe EnemyId
getTarget (UIState {rightPaneTarget}) =
  case rightPaneTarget of
       RPEnemy eid -> Just eid
       _ -> Nothing

--------------------------------------------------------------------------------
-- Actions ---------------------------------------------------------------------
--------------------------------------------------------------------------------

doAction :: GameAction -> Instant -> UIState -> GameState -> UI Unit
doAction action time uis gs = do
  result <- F.action action
  let gs' = either (const gs) identity result
      uis' = nextUI time gs result uis
  runUI uis' gs'

tryAttack :: Instant -> Vector Int -> UIState -> GameState -> UI Unit
tryAttack t pos uis gs =
  case getTarget uis of
       Nothing -> runUI uis gs
       Just tid -> doAction (Attack pos tid) t uis gs

canInstallOrgan :: Vector Int -> InternalOrgan -> GameState -> Boolean
canInstallOrgan pos organ (GameState g) =
 let bag = g.playerHealth
       # un Health
       # _.board
       # un Board
       # _.organs
     o = Tuple.fst organ
  in canInsertOrgan pos o bag && all isValidBoardCoord (extent o pos)

tryInstallOrgan :: Instant -> Vector Int -> InternalOrgan
    -> UIState -> GameState -> UI Unit
tryInstallOrgan t pb organ uis gs@(GameState g) =
  if canInstallOrgan pb organ gs
  then audioAction "Organ2.mp3" (InstallOrgan organ pb) t uis gs
  else runUI uis gs

tryRemoveOrgan :: Instant -> Vector Int -> UIState -> GameState -> UI Unit
tryRemoveOrgan t p uis gs@(GameState g) =
  let b = (g.playerHealth # un Health # _.board # un Board # _.organs)
   in case organAt p b of
        Nothing -> runUI uis gs
        Just (Tuple _ pos) ->
          audioAction "Organ3.mp3" (RemoveOrgan pos) t
            (setDirtyUI t uis) gs

{-
removeOrgan :: Instant -> Vector Int -> UIState -> GameState -> UI Unit
removeOrgan
                          audioAction "Organ3.mp3" (RemoveOrgan pos) time
                            (setDirtyUI time uis) gs

-}

audioAction :: String -> GameAction -> Instant -> UIState -> GameState -> UI Unit
audioAction audio action time uis gs = do
  result <- F.action action
  let gs' = either (const gs) identity result
      uis' = nextUI time gs result uis
             # enqueueAudio time audio
  runUI uis' gs'

--------------------------------------------------------------------------------

setDirtyGS :: Instant -> UIState -> UIState
setDirtyGS t (UIState u) = UIState u {gsTimestamp = t}

setDirtyUI :: Instant -> UIState -> UIState
setDirtyUI t (UIState u) = UIState u {timestamp = t}

setDirtyAll :: Instant -> UIState -> UIState
setDirtyAll t uis = uis
  # setDirtyUI t
  # setDirtyGS t

nextUI
  :: Instant
  -> GameState
  -> Either FailedAction GameState
  -> UIState
  -> UIState
nextUI t gs a uis = updateUIState t gs a (clearAudio uis)

updateUIState
  :: Instant
  -> GameState
  -> Either FailedAction GameState
  -> UIState
  -> UIState
updateUIState t
  (GameState {p})
  (Left (FailedAction direction))
  (UIState uis) =
  let bump = A.bump t (DiffTime 200.0) (move direction zero)
   in UIState $ uis
      { timestamp = t
      , playerAnim = A.prune t $ uis.playerAnim + bump
      }
updateUIState t
  _
  (Right (GameState {events}))
  (UIState uis) = foldr (uiEvent t) ui events
  where
    ui = UIState uis
         { timestamp = t
         , gsTimestamp = t
         }
updateUIState t
  (GameState {p})
  (Left _)
  uis = uis

uiEvent :: Instant -> Event -> UIState -> UIState
uiEvent t (PlayerMoved dir) (UIState uis) =
  let backwards = ((*) tileSize) <$> dirVector (opposite dir)
      -- create an animating vector that initially points from player's
      -- current position to previous position, but once settled is
      -- equal to zero
      dur = DiffTime 300.0
      fade = A.reverseSlide t dur backwards
   in UIState uis {
        playerAnim = A.prune t $ uis.playerAnim + fade
      }
uiEvent t (EnemyMoved eid vec) (UIState uis) =
  let cleanedAnim = uis.enemyAnim
        -- prune all ongoing animations of extra stuff
        # map (A.prune t)
        -- remove all completed animations
        -- (they all complete to zero-vectors)
        # Map.filter (not <<< A.isStatic t)
      dur = DiffTime 300.0
      realVec = ((*) tileSize) <<< toNumber <$> vec
      fade = A.reverseSlide t dur (negate realVec)
      anim = case Map.lookup eid cleanedAnim of
                  Nothing -> fade
                  Just a -> a + fade
   in
  UIState uis {
    enemyAnim = Map.insert eid anim cleanedAnim
  }
uiEvent t (PlayerAttacked _) uis = uis
  # enqueueAudio t "pew1.mp3"
uiEvent t _ uis = uis

getDir :: String -> Maybe Direction
getDir "ArrowLeft" = Just left
getDir "ArrowRight" = Just right
getDir "ArrowDown" = Just down
getDir "ArrowUp" = Just up
getDir _ = Nothing

type Offset = Animating (Vector Number)

--------------------------------------------------------------------------------
-- Dirty flagging --------------------------------------------------------------
--------------------------------------------------------------------------------

-- For each part of the UI, we keep track of the timestamp of the most recent
-- change to the non-animated part of it

--TODO


--------------------------------------------------------------------------------
-- UI dimensions ---------------------------------------------------------------
--------------------------------------------------------------------------------

cutLeft :: Number -> Rectangle -> Rectangle
cutLeft d r =
  r { width = d
    }

cutRight :: Number -> Rectangle -> Rectangle
cutRight d r =
  r { x = r.x + d
    , width = r.width - d
    }

cutUp :: Number -> Rectangle -> Rectangle
cutUp d r =
  r { height = d
    }

cutDown :: Number -> Rectangle -> Rectangle
cutDown d r =
  r { y = r.y + d
    , height = r.height - d
    }

screen :: Rectangle
screen =
  { x: 0.0
  , y: 0.0
  , width: (leftPaneTiles + centerPaneTiles + rightPaneTiles) * tileSize
               + leftPaneBorder + rightPaneBorder
  , height: verticalTiles * tileSize
  }

leftPaneSize :: Number
leftPaneSize = leftPaneTiles * tileSize + leftPaneBorder

leftPaneRect :: Rectangle
leftPaneRect = cutLeft leftPaneSize screen

playerBoardRect :: Rectangle
playerBoardRect = leftPaneRect
  # cutDown 11.0
  # cutUp (6.0 * tileSize)

centerPaneSize :: Number
centerPaneSize = centerPaneTiles * tileSize

centerPaneRect :: Rectangle
centerPaneRect = screen
  # cutRight leftPaneSize
  # cutLeft centerPaneSize

rightPaneRect :: Rectangle
rightPaneRect = screen
  # cutRight (leftPaneSize + centerPaneSize)

targetNameSize :: Number
targetNameSize = 3.0 * tileSize

targetNameRect :: Rectangle
targetNameRect = rightPaneRect
  # cutUp targetNameSize

targetBoardContainerSize :: Number
targetBoardContainerSize = rightPaneTiles * tileSize

targetBoardContainerRect :: Rectangle
targetBoardContainerRect = rightPaneRect
  # cutDown targetNameSize
  # cutUp targetBoardContainerSize

targetPadding :: Number
targetPadding = (rightPaneTiles - 6.0) * tileSize

targetBoardRect :: Rectangle
targetBoardRect = targetBoardContainerRect
  # cutDown targetPadding
  # cutRight targetPadding

targetDimensions :: { width :: Number, height :: Number }
targetDimensions = { width: screen.width, height: screen.height }

leftPaneTiles :: Number
leftPaneTiles = 6.0

leftPaneBorder :: Number
leftPaneBorder = 1.0

rightPaneBorder :: Number
rightPaneBorder = 1.0

centerPaneTiles :: Number
centerPaneTiles = 40.0

rightPaneTiles :: Number
rightPaneTiles = 9.0

verticalTiles :: Number
verticalTiles = 40.0

tileSize :: Number
tileSize = 10.0

--------------------------------------------------------------------------------
-- Locating clicks -------------------------------------------------------------
--------------------------------------------------------------------------------

data UILocation =
  TargetBoard (Vector Int)
  | CenterPane (Vector Int)
  | PlayerBoard (Vector Int)
  | Other

-- If the position is inside the rect, returns
-- rect-local coordinates for that position
inRect :: Vector Number -> Rectangle -> Maybe (Vector Number)
inRect (V v) r =
  let x = v.x - r.x
      y = v.y - r.y
   in if    x >= 0.0 && x <= r.width
         && y >= 0.0 && y <= r.height
      then Just (V{x,y})
      else Nothing

locate :: Vector Number -> UILocation
locate p = fromMaybe Other $
      locateBlock p CenterPane centerPaneRect
  <|> locateBlock p TargetBoard targetBoardRect
  <|> locateBlock p PlayerBoard playerBoardRect

locateBlock
  :: Vector Number
  -> (Vector Int -> UILocation)
  -> Rectangle
  -> Maybe UILocation
locateBlock p f rect = do
  (V v) <- inRect p rect
  pure $ f $ V
         { x: Int.floor (v.x / tileSize)
         , y: Int.floor (v.y / tileSize)
         }

--------------------------------------------------------------------------------
-- Audio stuff -----------------------------------------------------------------
--------------------------------------------------------------------------------

type AudioData = Map String FA.Audio

loadAudioData :: Array String -> Effect (Map String FA.Audio)
loadAudioData paths = do
  audios <- traverse FA.loadAudio paths
  pure $ Map.fromFoldable (Array.zip paths audios)

resolveAudio :: String -> UIState -> FA.Audio
resolveAudio path (UIState {audioData}) = unsafeFromJust (Map.lookup path audioData)

type AudioQueue = Array { file :: String, time :: Instant }

emptyAudioQueue :: AudioQueue
emptyAudioQueue = []

clearAudio :: UIState -> UIState
clearAudio (UIState u) = UIState u { audioQueue = emptyAudioQueue }

enqueueAudio :: Instant -> String -> UIState -> UIState
enqueueAudio time file (UIState u) = UIState u
  { audioQueue = Array.cons {time, file} u.audioQueue
  , timestamp = time
  }


getAudio :: UIState -> FA.AudioSignal
getAudio u@(UIState uis) =
  { samples, timestamp: Just uis.timestamp }
  where
    samples = Array.filter (\{delay} -> delay >= 0.0) $
              uis.audioQueue <#> \{ file, time } ->
                { audio: resolveAudio file u
                , delay: un Milliseconds (unInstant time)
                       - un Milliseconds (unInstant uis.timestamp)
                }

--------------------------------------------------------------------------------
-- Highlights ------------------------------------------------------------------
--------------------------------------------------------------------------------

rectPos :: Rectangle -> Vector Number
rectPos {x,y} = V {x,y}

fromGrid :: Vector Int -> Vector Number
fromGrid p = p <#> \x -> toNumber x * tileSize

tileRect :: Vector Number -> Rectangle
tileRect (V{x,y}) = {x,y,width:tileSize,height:tileSize}

toRect :: UILocation -> Maybe Rectangle
toRect (TargetBoard p) = Just $ tileRect $ fromGrid p + rectPos targetBoardRect
toRect (CenterPane p) = Just $ tileRect $ fromGrid p + rectPos centerPaneRect
toRect (PlayerBoard p) = Just $ tileRect $ fromGrid p + rectPos playerBoardRect
toRect Other = Nothing

clearHighlights :: UIState -> UIState
clearHighlights (UIState u) = UIState u{highlights = []}

highlight :: UILocation -> UIState -> UIState
highlight p uis = fromMaybe uis do
  rect <- toRect p
  pure $ hightlightRect rect uis

highlights :: Array UILocation -> UIState -> UIState
highlights ps uis = foldl (\u p -> highlight p u) uis ps

hightlightRect :: Rectangle -> UIState -> UIState
hightlightRect r (UIState u) = UIState u{highlights = Array.cons r u.highlights}

--------------------------------------------------------------------------------
-- Image paths -----------------------------------------------------------------
--------------------------------------------------------------------------------

imagePaths :: Array String
imagePaths =
  [ "player.png"
  , "ButtonPushed.png"
  , "ButtonUnpushed.png"
  , "ground.png"
  , "heal.png"
  , "heart.png"
  , "injuredheart.png"
  , "player.png"
  , "roomba.png"
  , "wall.png"
  , "Heart4.png"
  , "Heart4injured.png"
  , "placeholder.png"
  ]

audioPaths :: Array String
audioPaths =
  [ "Clang1.mp3"
  , "Organ1.mp3"
  , "Organ2.mp3"
  , "Organ3.mp3"
  , "Puip1.mp3"
  , "Taser1.mp3"
  , "Tink1.mp3"
  , "Tink2.mp3"
  , "pew1.mp3"
  ]
