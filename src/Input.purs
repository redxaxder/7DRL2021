module Input where

import Extra.Prelude

import FRP.Event (Event, makeEvent)
import FRP.Event.Time (withTime, debounce)
import Graphics.Canvas (CanvasElement)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.Event.Event as Web

import PointerEvent (PointerEvent)
import PointerEvent as Ptr

import Web.UIEvent.KeyboardEvent  as KB

import Unsafe.Coerce (unsafeCoerce)
import Data.Variant as V
import Data.Time.Duration (Milliseconds (..))
import Control.Alt ((<|>))

type Input = { time :: Instant, value :: InputValue }

data InputValue =
    KeyDown KeyDown
  | PointerDown Ptr
  | PointerUp Ptr
  | PointerMove Ptr

type KeyDown = String

repeatDelay :: Milliseconds
repeatDelay = Milliseconds 200.0

type Ptr = { location :: Vector Number, pointerId :: Number }

unpack :: PointerEvent -> Ptr
unpack p =
  { location: V { x: Ptr.offsetX p, y: Ptr.offsetY p }
  , pointerId: Ptr.pointerId p
  }

getInputs :: CanvasElement -> Event Input
getInputs c = withTime $
  (KeyDown <$> keyDown c)
  <|> (PointerDown <<< unpack <$> pointerDown c)
  <|> (PointerUp <<< unpack <$> pointerUp c)
  <|> (PointerMove <<< unpack <$> pointerMove c)

keyDown :: CanvasElement -> Event KeyDown
keyDown = debounce repeatDelay <<< mkEventListener "keydown" (KB.fromEvent >>> map KB.code)

pointerDown :: CanvasElement -> Event PointerEvent
pointerDown cs = debounce (Milliseconds 500.0) $
   mkEventListener "pointerdown" Ptr.fromEvent cs

pointerUp :: CanvasElement -> Event PointerEvent
pointerUp = debounce repeatDelay <<< mkEventListener "pointerup" Ptr.fromEvent

pointerMove :: CanvasElement -> Event PointerEvent
pointerMove = mkEventListener "pointermove" Ptr.fromEvent

-- TODO: think about pointer leave/out/cancel/whatever

mkEventListener :: forall a
  . String
  -> (Web.Event -> Maybe a)
  -> CanvasElement
  -> Event a
mkEventListener eventName parseEvent c = 
 makeEvent \k -> do
  let target = unsafeCoerce c
  listener <- eventListener \e -> do
    parseEvent e # traverse_ k
  addEventListener (wrap eventName) listener false target
  pure (removeEventListener (wrap eventName) listener false target)
