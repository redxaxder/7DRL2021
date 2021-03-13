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

import GameState (GameState, GameAction, FailedAction, initState, step)
import UI (imagePaths, UIState, runUI, targetDimensions, initUIState, getAudio)

main :: Effect Unit
main = unsafePartial $ launchAff_ $ do
  liftEffect $ C.log "..."
  -- initialize canvas
  init <- liftEffect $ initState
  let { width, height } = targetDimensions
  cv <- fromJust <$> Canvas.init { width, height, canvasId: "canvas", imagePaths }
  ctx <- liftEffect $ newRendererState cv
  uiState <- liftEffect $ initUIState init
  let engineConfig :: EngineConfig GameState GameAction FailedAction UIState Input RendererState
      engineConfig =
          { inputs: getInputs cv.canvas
          , ui: runUI uiState init
          , init
          , step
          , ctx
          , draw
          , getAudio
          }
  -- run engine
  cancel <- liftEffect $ runEngine engineConfig
  pure unit

