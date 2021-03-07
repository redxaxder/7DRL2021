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

type InputValue = V.Variant
   ( keyDown :: KeyDown
   , pointerDown :: PointerEvent
   , pointerUp :: PointerEvent
   , pointerMove :: PointerEvent
   )

type KeyDown = String


repeatDelay :: Milliseconds
repeatDelay = Milliseconds 200.0

getInputs :: CanvasElement -> Event Input
getInputs c = withTime $
  (V.inj (SProxy :: SProxy "keyDown") <$> keyDown c)
  <|> (V.inj (SProxy :: SProxy "pointerDown") <$> pointerDown c)
  <|> (V.inj (SProxy :: SProxy "pointerUp") <$> pointerUp c)
  <|> (V.inj (SProxy :: SProxy "pointerMove") <$> pointerMove c)

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
