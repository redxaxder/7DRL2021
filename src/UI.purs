module UI where

import Extra.Prelude

import Data.Either (either)
import Data.Variant as V
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
import Data.Board (BoardCoord (..))
import Data.Terrain (Terrain)
import GameState
  ( GameState (..)
  , GameAction (..)
  , FailedAction (..)
  , getTargetAtPosition
  , Target (..)
  , EnemyId
  , Event (..)
  )

import Input (Input)
import PointerEvent as Ptr



type UI r =
  F.UIM GameState GameAction FailedAction UIState Input r

newtype UIState = UIState
  { rightPaneTarget :: RightPane
  , lockedTarget :: RightPane
  , timestamp :: Instant
  , gsTimestamp :: Instant
  , playerAnim :: Offset
  , enemyAnim :: Map EnemyId Offset
  , audioData :: AudioData
  , audioQueue :: AudioQueue
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
    , enemyAnim: Map.empty
    , audioData
    , audioQueue: []
    }

{-
getDragOffset ::
   { pointerId :: Number
   , start :: Vector Number
   , current :: Vector Number
   } -> UIState -> GameState -> UI (Maybe (Vector Number))
getDragOffset conf uis@(UIState baseUI) gs = do
  let default = getDragOffset conf uis gs
  { time, value } <- F.input (UIState baseUI{ pointerState = Dragging conf })
  V.default default
    # V.onMatch
      { pointerDown: \_ -> pure Nothing
      , pointerMove: \ptr ->
          let ptrLoc = V { x: Ptr.offsetX ptr, y: Ptr.offsetY ptr }
           in if Ptr.pointerId ptr == conf.pointerId
              then getDragOffset conf{current = ptrLoc} uis gs
              else default
      , pointerUp: \ptr ->
          let ptrLoc = V { x: Ptr.offsetX ptr, y: Ptr.offsetY ptr }
           in if Ptr.pointerId ptr == conf.pointerId
              then pure $ Just (ptrLoc - conf.start)
              else default
      }
    $ value
-}

runUI :: UIState -> GameState -> UI Unit
runUI currentUI@(UIState u) gs = do
  let default _ = runUI currentUI gs
  { time, value } <- F.input currentUI
  V.default (default unit)
    # V.onMatch
      { keyDown: \key -> case getDir key of
           Nothing -> default unit
           Just d -> doAction (Move d) time currentUI gs
      , pointerDown: \ptr ->
          let ptrLoc = V { x: Ptr.offsetX ptr, y: Ptr.offsetY ptr }
           in case locate ptrLoc of
                Other -> default unit
                CenterPane p ->
                   let newUI = lockTarget time p currentUI gs
                    in runUI newUI gs
                TargetBoard p ->
                   tryAttack time p currentUI gs
      , pointerMove: \ptr ->
          let ptrLoc = V { x: Ptr.offsetX ptr, y: Ptr.offsetY ptr }
           in case locate ptrLoc of
                CenterPane p ->
                  let newUI = viewTarget time p currentUI gs
                   in runUI newUI gs
                _ -> if u.lockedTarget /= u.rightPaneTarget
                       then let newUI = toLockedTarget time currentUI
                            in runUI newUI gs
                       else default unit
      }
    $ value

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

tryAttack :: Instant -> Vector Int -> UIState -> GameState -> UI Unit
tryAttack t pos uis gs =
  case getTarget uis of
       Nothing -> runUI uis gs
       Just tid -> doAction (Attack (BoardCoord pos) tid) t uis gs

doAction :: GameAction -> Instant -> UIState -> GameState -> UI Unit
doAction action time uis gs = do
  result <- F.action action
  let gs' = either (const gs) identity result
      uis' = updateUIState time gs result uis
  runUI uis' gs'

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
locate p = fromMaybe Other $ locateCenterPane p <|> locateTargetBoard p

locateCenterPane :: Vector Number -> Maybe UILocation
locateCenterPane p = do
  (V v) <- inRect p centerPaneRect
  pure $ CenterPane $ V
         { x: Int.floor (v.x / tileSize)
         , y: Int.floor (v.y / tileSize)
         }

locateTargetBoard :: Vector Number -> Maybe UILocation
locateTargetBoard p = do
  (V v) <- inRect p targetBoardRect
  pure $ TargetBoard $ V
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
  { audioQueue = Array.cons {time, file} u.audioQueue }

getAudio :: UIState -> FA.AudioSignal
getAudio u@(UIState uis) =
  { samples, timestamp: Just uis.timestamp }
  where
    samples = uis.audioQueue <#> \{ file, time } ->
                { audio: resolveAudio file u
                , delay: un Milliseconds (unInstant time)
                       - un Milliseconds (unInstant uis.timestamp)
                }


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
  [ "pew1.mp3"
  ]
