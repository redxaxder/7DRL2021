module Framework.Audio where

import Extra.Prelude

foreign import data Audio :: Type

foreign import loadAudio :: String -> Effect Audio

foreign import playAudio :: Number -> Audio -> Effect Unit

type AudioSignal =
  { timestamp :: Maybe Instant
  , samples :: Array { audio :: Audio, delay :: Number }
  }

emptyAudioSignal :: AudioSignal
emptyAudioSignal =
  { timestamp: Nothing
  , samples: []
  }
