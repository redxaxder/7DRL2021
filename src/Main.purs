module Main where

import Extra.Prelude

import Effect.Console as C
import Effect.Class (liftEffect)

import Effect.Aff (launchAff_)


import Partial.Unsafe (unsafePartial)


import Framework.Engine (runEngine, EngineConfig)
import Framework.Render.Canvas as Canvas


import Input (Input, getInputs)

import Graphics.Render (draw, RendererState, newRendererState)

import GameState (GameState, GameAction, FailedAction, newState, step)
import UI (imagePaths, UIState, mainScreen, targetDimensions)

main :: Effect Unit
main = unsafePartial $ launchAff_ $ do
  liftEffect $ C.log "..."
  -- initialize canvas
  init <- liftEffect $ newState
  liftEffect $ C.log "..."
  let { width, height } = targetDimensions
  liftEffect $ C.log "..."
  cv <- fromJust <$> Canvas.init { width, height, canvasId: "canvas", imagePaths }
  ctx <- liftEffect $ newRendererState cv
  liftEffect $ C.log "..."
  let engineConfig :: EngineConfig GameState GameAction FailedAction UIState Input RendererState
      engineConfig =
          { inputs: getInputs cv.canvas
          , ui: mainScreen init
          , init
          , step
          , ctx
          , draw
          }
  -- run engine
  liftEffect $ C.log "..."
  cancel <- liftEffect $ runEngine engineConfig
  liftEffect $ C.log "....."
  pure unit

