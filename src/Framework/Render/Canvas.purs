module Framework.Render.Canvas 
  ( runRenderCanvas
  , Config
  , init
  , Vars
  , ImageData
  ) where

import Extra.Prelude

import Graphics.Canvas as Canvas
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Effect.Aff (Aff, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)

import Data.Map as Map

import Data.Array as Array

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader.Trans (ReaderT (..), runReaderT)

import Control.Monad.Free as Free

import Framework.Render.Color (Color(..))
import Framework.Render.Core (Image(..), RenderF(..), RenderM)

type Config =
  { width :: Number
  , height :: Number
  , canvasId :: String
  , imagePaths :: Array String
  }

type ImageData =
  { index :: Ref (Map String Image)
  , array :: Ref (Array Canvas.CanvasImageSource)
  }

type Vars =
  { canvas :: Canvas.CanvasElement
  , imageData :: ImageData
  }

init :: Config -> Aff (Maybe Vars)
init { width, height, canvasId, imagePaths } = runMaybeT do
  canvas <- MaybeT $ liftEffect $ Canvas.getCanvasElementById canvasId
  liftEffect $ Canvas.setCanvasDimensions canvas { width, height }
  context <- liftEffect $ Canvas.getContext2D canvas
  liftEffect $ setImageSmoothing context false
  index <- liftEffect $ Ref.new Map.empty
  array <- liftEffect $ Ref.new mempty
  let imageData = {index, array}
  lift $ for_ imagePaths \p -> fetchImage p imageData
  pure $ { canvas, imageData }


fetchImage :: String -> ImageData -> Aff Unit
fetchImage path {index, array}= do
  images <- liftEffect $ Ref.read index
  case Map.lookup path images of
       Just i -> pure unit
       Nothing -> do
         imgsrc <- makeAff \h -> do
            Canvas.tryLoadImage path (h <<< maybe (Left $ error $ "failed to load image " <> path) pure)
            mempty
         void $ liftEffect $ do
            nextIndex <- Array.length <$> Ref.read array
            Ref.write (Map.insert path (Image nextIndex) images) index
            Ref.modify (flip Array.snoc imgsrc) array


foreign import setImageSmoothing :: Canvas.Context2D -> Boolean -> Effect Unit


runRenderCanvas :: forall a. Vars -> RenderM a -> Effect a
runRenderCanvas vars f = runReaderT (Free.runFreeM (ReaderT <<< go) f) vars
  where
  go :: RenderF (RenderM a) -> Vars -> Effect (RenderM a)
  go (LoadImage path k) {imageData: {index}} = do
     images <- liftEffect $ Ref.read index
     let img = Map.lookup path images
     pure $ k $ img

  go (GetDimensions k) { canvas } = do
     width <- Canvas.getCanvasWidth canvas
     height <- Canvas.getCanvasHeight canvas
     pure $ k { width, height }

  go (FillRect (Color color) rect next) { canvas } = do
     ctx <- Canvas.getContext2D canvas
     Canvas.withContext ctx $ do
       Canvas.setFillStyle ctx color
       Canvas.fillRect ctx rect
     pure next

  go (DrawLine {color, start, end, width} next) {canvas} = do
     ctx <- Canvas.getContext2D canvas
     Canvas.withContext ctx $ do
        Canvas.beginPath ctx
        Canvas.setStrokeStyle ctx (un Color color)
        Canvas.setLineWidth ctx width
        Canvas.moveTo ctx start.x start.y
        Canvas.lineTo ctx end.x end.y
        Canvas.stroke ctx
     pure next

  go (DrawImage (Image i) {x,y, width, height} next) { canvas, imageData } = do
     images <- Ref.read imageData.array
     ctx <- Canvas.getContext2D canvas
     case Array.index images i of
          Nothing -> throwError (error "Missing image!")
          Just img -> Canvas.drawImageScale ctx img x y width height
     pure next


  go (DrawImageFull (Image i) s d next) { canvas, imageData } = do
     images <- Ref.read imageData.array
     ctx <- Canvas.getContext2D canvas
     case Array.index images i of
          Nothing -> throwError (error "Missing image!")
          Just img -> Canvas.drawImageFull ctx img
            s.x s.y s.width s.height
            d.x d.y d.width d.height
     pure next
