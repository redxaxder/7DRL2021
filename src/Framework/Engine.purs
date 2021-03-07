module Framework.Engine where

import Prelude

import Effect (Effect)
import Control.Monad.Rec.Class (tailRec, Step(..))
import FRP.Event (create, subscribe, sampleOn, Event)
import FRP.Event.Time (withTime, interval)
import Framework.UI (UIM, UIF (..), getUIState)
import Data.DateTime.Instant (Instant)
import Control.Monad.Free as Free
import Data.Either as Either
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Filterable (filterMap)

type EngineConfig gs a e ui i ctx =
  { inputs :: Event i
  , ui :: UIM gs a e ui i Unit
  , init :: gs
  , step :: gs -> a -> Either e gs
  , ctx :: ctx
  , draw :: Instant -> ui -> gs -> ctx -> Effect Unit
  }

runEngine
  :: forall gs a e ui i ctx
   . EngineConfig gs a e ui i ctx
  -> Effect (Effect Unit)
runEngine { inputs, ui: uiInit, init, step, ctx, draw } = do
  { event: engineState, push: pushEngineState } <- create
  -- redraw screen
  let tick = interval 33 -- 33 milliseconds -> 30 FPS
  let tickWithEngineState = sampleOn engineState $ (flip const <$> tick)
  cancelDraw <-
     subscribe
     (tickWithEngineState
       # (filterMap \{ui, gs} -> do
             uis <- getUIState ui
             pure {uis, gs}
           )
       # withTime
     )
     \{ time, value: {uis, gs}} -> draw time uis gs ctx
  -- step the game in response to user actions
  let freshEngine = { ui: uiInit, gs: init }
  let handleReset Nothing = freshEngine
      handleReset (Just e) = e
  cancelEngine <-
    subscribe
    (sampleOn engineState $
      (stepEngine freshEngine step <$> inputs)
    )
    pushEngineState
  pushEngineState freshEngine
  pure $ do
     cancelEngine
     cancelDraw

type EngineState gs a e ui i =
  { ui :: UIM gs a e ui i Unit
  , gs :: gs
  }

type R gs a e ui i =
  { ui :: UIM gs a e ui i Unit
  , gs :: gs
  , i :: Maybe i
  }

stepEngine :: forall gs a e ui i
  .  EngineState gs a e ui i
  -> (gs -> a -> Either e gs)
  -> i
  -> EngineState gs a e ui i
  -> EngineState gs a e ui i
stepEngine reset step input {ui: u, gs: g} = tailRec go
  { ui: u, gs: g, i: Just input }
  where
    go :: R gs a e ui i
       -> Step (R gs a e ui i) (EngineState gs a e ui i)
    go { ui, gs, i } =
      case Free.resume ui of
           (Right _) -> Done reset
           (Left (UIAction a k)) ->
             let result = step gs a
              in Loop { ui: k result
                      , gs: Either.either (pure gs) identity result
                      , i
                      }
           (Left (UIAwaitingInput _ k)) ->
             case i of
                  Nothing -> Done { ui, gs }
                  (Just x) ->
                    Loop { ui: k x
                         , gs
                         , i: Nothing
                         }
