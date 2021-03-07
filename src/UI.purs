module UI where

import Extra.Prelude

import Framework.Direction (down, left, right, up, move, Direction)
import Framework.UI as F

import Animation (Animating, DiffTime(..))
import Animation as A
import GameState (GameState (..), GameAction (..), FailedAction (..))
import Input (Input)
import Data.Variant as V

import Data.Either (either)
import PointerEvent as Ptr

type UI r =
  F.UIM GameState GameAction FailedAction UIState Input r

newtype UIState = UIState
  { element :: Element
  , pointerState :: PointerState
  }

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
   in UIState $ uis{
      element = Element $ e { pos = map (A.prune t) e.pos + bump }
      }
updateUIState t
  (GameState {p})
  (Right (GameState {p:p'}))
  (UIState uis@{element: (Element e)}) =
  let dp = toNumber <$> p' - p
      dpAnimation = A.slide t (DiffTime 300.0) dp
   in UIState uis{
      element = Element $ e { pos = map (A.prune t) e.pos + dpAnimation }
      }

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

imagePaths :: Array String
imagePaths = [ "player.png" ]
