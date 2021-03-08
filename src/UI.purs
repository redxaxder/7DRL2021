module UI where

import Extra.Prelude

import Framework.Direction (down, left, right, up, move, Direction)
import Framework.UI as F

import Animation (Animating, DiffTime(..))
import Animation as A
import GameState
  ( GameState (..)
  , GameAction (..)
  , FailedAction (..)
  , EnemyId
  )
import Input (Input)
import Data.Variant as V

import Data.Either (either)
import PointerEvent as Ptr

import Framework.Render.Core (Rectangle)

import Data.DateTime.Instant (instant)
import Data.Time.Duration (Milliseconds (..))

type UI r =
  F.UIM GameState GameAction FailedAction UIState Input r

newtype UIState = UIState
  { element :: Element
  , pointerState :: PointerState
  , rightPaneTarget :: V.Variant RightPane
  , timestamp :: Instant
  , gsTimestamp :: Instant
  }

type RightPane =
  ( enemy :: EnemyId
  , floor :: Unit
  , wall :: Unit
  , none :: Unit
  )

data PointerState = Neutral
  | Dragging
    { pointerId :: Number
    , start :: Vector Number
    , current :: Vector Number
    }

initUIState :: GameState -> UIState
initUIState (GameState {p}) = UIState
  { element: Element
    { image: "player.png"
    , pos: pure <<< toNumber <$> p
    }
  , pointerState: Neutral
  , rightPaneTarget: V.inj (SProxy :: SProxy "none") unit
  , timestamp: unsafeFromJust $ instant $ Milliseconds 0.0
  , gsTimestamp: unsafeFromJust $ instant $ Milliseconds 0.0
  }

mainScreen :: GameState -> UI Unit
mainScreen gs = runUI (initUIState gs) gs

data HitResult target =
    NoHit
  | HitClickable target
  | HitDraggable target

hitTest :: UIState -> GameState -> Vector Number -> HitResult Unit
hitTest uis gs loc = spy (show loc) NoHit

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

runUI :: UIState -> GameState -> UI Unit
runUI currentUI gs = do
  let default _ = runUI currentUI gs
  { time, value } <- F.input currentUI
  V.default (default unit)
    # V.onMatch
      { keyDown: \key -> case getDir key of
           Nothing -> default unit
           Just d -> do
             result <- F.action (Move d)
             let gs' = either (const gs) identity result
             runUI (updateUIState time gs result currentUI) gs'
      , pointerDown: \ptr ->
          let ptrLoc = V { x: Ptr.offsetX ptr, y: Ptr.offsetY ptr }
           in case hitTest currentUI gs ptrLoc of
              NoHit -> default unit
              HitClickable _ -> default unit
              HitDraggable _ -> do
                 dragOffset <- getDragOffset
                    { pointerId: Ptr.pointerId ptr
                    , start: ptrLoc
                    , current: ptrLoc
                    } currentUI gs
                 case dragOffset of
                      Nothing -> default unit
                      Just result -> default $ spy (show result) unit
      }
    $ value

updateUIState
  :: Instant
  -> GameState
  -> Either FailedAction GameState
  -> UIState
  -> UIState
updateUIState t
  (GameState {p})
  (Left (FailedAction direction))
  (UIState uis@{element: (Element e)}) =
  let bump = A.bump t (DiffTime 200.0) (move direction zero)
   in UIState $ uis
      { element = Element $ e { pos = map (A.prune t) e.pos + bump }
      , timestamp = t
      }
updateUIState t
  (GameState {p})
  (Right (GameState {p:p'}))
  (UIState uis@{element: (Element e)}) =
  let dp = toNumber <$> p' - p
      dpAnimation = A.slide t (DiffTime 300.0) dp
   in UIState uis
      { element = Element $ e { pos = map (A.prune t) e.pos + dpAnimation }
      , timestamp = t
      , gsTimestamp = t
      }
updateUIState t
  (GameState {p})
  (Left _)
  uis = uis

getDir :: String -> Maybe Direction
getDir "ArrowLeft" = Just left
getDir "ArrowRight" = Just right
getDir "ArrowDown" = Just down
getDir "ArrowUp" = Just up
getDir _ = Nothing

data Element = Element
  { image :: String
  , pos :: Vector (Animating Number)
  }

--------------------------------------------------------------------------------
-- UI dimensions ---------------------------------------------------------------
--------------------------------------------------------------------------------

leftPaneRect :: Rectangle
leftPaneRect = { x:0.0,y:0.0
  , width: leftPaneTiles * tileSize + leftPaneBorder
  , height: totalHeight
  }

centerPaneRect :: Rectangle
centerPaneRect =
  { x: leftPaneRect.width
  , y: 0.0
  , width: centerPaneTiles * tileSize + rightPaneBorder
  , height: totalHeight
  }

rightPaneRect :: Rectangle
rightPaneRect =
  { x: centerPaneRect.x + centerPaneRect.width
  , y: 0.0
  , width: rightPaneTiles * tileSize
  , height: totalHeight
  }

targetDimensions :: { width :: Number, height :: Number }

targetDimensions = { width: totalWidth, height: totalHeight}

totalWidth :: Number
totalWidth = leftPaneRect.width + centerPaneRect.width + rightPaneRect.width

totalHeight :: Number
totalHeight = tileSize * verticalTiles

leftPaneTiles :: Number
leftPaneTiles = 6.0

leftPaneBorder :: Number
leftPaneBorder = 1.0

rightPaneBorder :: Number
rightPaneBorder = 1.0

centerPaneTiles :: Number
centerPaneTiles = 40.0

rightPaneTiles :: Number
rightPaneTiles = 10.0

verticalTiles :: Number
verticalTiles = 40.0

tileSize :: Number
tileSize = 10.0

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
  , "player.png"
  , "roomba.png"
  , "wall.png"
  , "Heart4.png"
  , "placeholder.png"
  ]
