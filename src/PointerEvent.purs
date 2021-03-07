module PointerEvent
  ( PointerEvent
  , fromUIEvent
  , fromEvent
  , toUIEvent
  , toEvent
  , pointerId
  , screenX
  , screenY
  , clientX
  , clientY
  , offsetX
  , offsetY
  , pageX
  , pageY
  , ctrlKey
  , shiftKey
  , altKey
  , metaKey
  , button
  , relatedTarget
  , buttons
  , getModifierState
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventTarget)
import Web.Internal.FFI (unsafeReadProtoTagged)
import Web.UIEvent.UIEvent (UIEvent)

foreign import data PointerEvent :: Type

fromUIEvent :: UIEvent -> Maybe PointerEvent
fromUIEvent = unsafeReadProtoTagged "PointerEvent"

fromEvent :: Event -> Maybe PointerEvent
fromEvent = unsafeReadProtoTagged "PointerEvent"

toUIEvent :: PointerEvent -> UIEvent
toUIEvent = unsafeCoerce

toEvent :: PointerEvent -> Event
toEvent = unsafeCoerce

foreign import pointerId :: PointerEvent -> Number

foreign import screenX :: PointerEvent -> Number

foreign import screenY :: PointerEvent -> Number

foreign import clientX :: PointerEvent -> Number

foreign import clientY :: PointerEvent -> Number

foreign import offsetX :: PointerEvent -> Number

foreign import offsetY :: PointerEvent -> Number

foreign import pageX :: PointerEvent -> Number

foreign import pageY :: PointerEvent -> Number

foreign import ctrlKey :: PointerEvent -> Boolean

foreign import shiftKey :: PointerEvent -> Boolean

foreign import altKey :: PointerEvent -> Boolean

foreign import metaKey :: PointerEvent -> Boolean

foreign import button :: PointerEvent -> Int

foreign import _relatedTarget :: PointerEvent -> Nullable EventTarget

relatedTarget :: PointerEvent -> Maybe EventTarget
relatedTarget = toMaybe <$> _relatedTarget

foreign import buttons :: PointerEvent -> Int

foreign import getModifierState
  :: String
  -> PointerEvent
  -> Effect Boolean
