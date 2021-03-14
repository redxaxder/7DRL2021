module Graphics.Render where

import Extra.Prelude

import Animation as A
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Map as Map
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds (..))
import Data.Int as Int
import Data.String as String
import Data.RevMap as RevMap
import Effect.Exception (error)
import Effect.Ref as Ref
import Math (round, sin)
import Math as Math
import Effect.Ref (Ref)
import Graphics.Canvas as Canvas
import Data.Set as Set
import Data.Traversable (scanl)
import Data.LinearIndex as LI
import Data.Enemy as Enemy
import Data.Board as Board
import Data.Tuple as Tuple
import Data.Board
  ( Board(..)
  , Clue (..)
  , Health (..)
  , Organ (..)
  , OrganType (..)
  , OrganSize (..)
  , getOrganAtPosition
  , getOrgans
  , organArray
  , hasRedEye
  , hasBlueEye
  )
import Data.Terrain
  ( Terrain(..)
  , blockPositions
  )

import Framework.Render.Canvas as FCanvas

import GameState
  ( GameState (..)
  , Level (..)
  , isSurgeryLevel
  , freshPlayerOrgans
  )
import UI
  ( UIState (..)
  , RightPane (..)
  , tileSize
  , screen
  , centerPaneRect
  , leftPaneRect
  , playerBoardRect
  --, leftPaneRect
  , rightPaneRect
  , targetBoardContainerRect
  , targetBoardRect
  )
import Framework.Render.Core (Rectangle, Image (..))

import Data.Enemy
  ( Enemy(..)
  , EnemyId
  )

import Data.Item as Item
import Data.Item
  ( Item(..)
  , ItemId
  )

{-



idea:
  each gamestate is timestamped
  draw function remembers the most recent timestamp it drew
  the static parts of a scene are drawn to a secondary buffer
  in an animated scene, the animation is cleaned up by copying from the secondary buffer
  parts of the draw are skipped



-}

newtype RendererState = RendererState
  { cvars :: FCanvas.Vars
  , gameStateId :: Ref (Maybe Instant)
  , uiStateId :: Ref (Maybe Instant)
  , toRestore :: Ref (Array Rectangle)
  , screenCache :: Ref Canvas.ImageData
  , debug :: Ref Debug
  }

type Debug = Int
initDebug :: Debug
initDebug = 0

resetDebug :: RendererState -> Effect Unit
resetDebug (RendererState r) = Ref.write initDebug r.debug

debugInfo :: RendererState -> Effect Unit
debugInfo (RendererState rs) = do
  -- y <- Ref.read rs.debug
  -- let _ = spy "d" y
  pure unit

note :: RendererState -> Effect Unit
note (RendererState rs) = Ref.modify_ (\x -> x + 1) rs.debug

--------------------------------------------------------------------------------
-- Screen caching and restoring ------------------------------------------------
--------------------------------------------------------------------------------
screenImageData :: FCanvas.Vars -> Effect Canvas.ImageData
screenImageData {canvas} = do
  ctx <- Canvas.getContext2D canvas
  Canvas.getImageData ctx screen.x screen.y screen.width screen.height

cacheScreen :: RendererState -> Effect Unit
cacheScreen (RendererState {cvars, screenCache}) = do
  imageData <- screenImageData cvars
  Ref.write imageData screenCache

restore :: RendererState -> Rectangle -> Effect Unit
restore (RendererState rs) rect = do
  ctx <- Canvas.getContext2D rs.cvars.canvas
  id <- Ref.read rs.screenCache
  {-
    the documentation for the function `putImageData` is all dumb and misleading
    the emperically determined behavior in chrome and firefox is as follows:
    it takes 6 arguments: ctx, imageData, dx, dy, dirtyX, dirtyY, dirtyW, dirtyH
    a rectangle is copied from the imageData to the ctx
    both rectangles have the same width and height, given by dirtyW and dirtyH
    the position of the source rectagle in imageData is given by:
       dirtyX, dirtyY
    the position of the target rectangle in the ctx is given by:
       dx + dirtyX, dy + dirtyY
    -}
  let dx = 0.0
      dy = 0.0
      dirtyX = rect.x
      dirtyY = rect.y
  Canvas.putImageDataFull ctx id
    dx dy
    dirtyX dirtyY
    rect.width rect.height

restoreAll :: RendererState -> Effect Unit
restoreAll rs@(RendererState {toRestore}) = do
  rects <- Ref.read toRestore
  for_ rects (restore rs)
  Ref.write [] toRestore

queueRestore :: RendererState -> Rectangle -> Effect Unit
queueRestore (RendererState {toRestore}) rect =
  Ref.modify_ (Array.cons rect) toRestore

drawImageTemp :: RendererState -> String -> Rectangle -> Effect Unit
drawImageTemp rs img rect = do
  drawImage rs img rect
  queueRestore rs {x: rect.x-1.0, y: rect.y -1.0
                  , width: rect.width+2.0, height: rect.height+2.0
                  }

fillRectTemp :: RendererState -> Rectangle -> Color -> Effect Unit
fillRectTemp rs rect color = do
  fillRect rs rect color
  queueRestore rs rect{width = rect.width + 1.0, height = rect.height + 1.0 }

--------------------------------------------------------------------------------

newRendererState :: FCanvas.Vars -> Effect RendererState
newRendererState cvars = do
  gameStateId <- Ref.new Nothing
  uiStateId <- Ref.new Nothing
  toRestore <- Ref.new []
  sid <- screenImageData cvars
  screenCache <- Ref.new sid
  debug <- Ref.new initDebug
  pure $ RendererState
    { cvars
    , gameStateId
    , uiStateId
    , toRestore
    , screenCache
    , debug
    }

draw :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
draw t uis@(UIState {timestamp, gsTimestamp}) gs@(GameState g) rs@(RendererState r) = do
  prevUI <- Ref.read r.uiStateId
  resetDebug rs
  restoreAll rs
  let uiDirty = maybe true ((>) timestamp) prevUI
  drawCenterPane t uis gs rs
  when uiDirty $ do
    drawPlayerBoard t uis gs rs
    drawRightPane t uis gs rs
    Ref.write (Just timestamp) r.uiStateId
  drawHighlights t uis rs
  drawDraggedOrgan uis gs rs
  debugInfo rs

drawPlayerBoard :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawPlayerBoard t uis (GameState gs) rs = do
  let playerBoard = (un Health gs.playerHealth).board
      playerHp = (un Health gs.playerHealth).hpCount
      playerOrgans = getOrgans playerBoard
      anchor = let {x,y} = playerBoardRect in V{x,y}
  clear rs leftPaneRect
  drawStats rs (GameState gs)
  drawBoardBase rs playerBoard playerBoardRect
  for_ playerOrgans.intact \(Tuple organ bc) ->
    drawOrgan true rs organ (fromGrid bc + anchor)
  for_ playerOrgans.injured \(Tuple organ bc) ->
    drawOrgan false rs organ (fromGrid bc + anchor)
  if playerHp <= 0
  then drawDeathMessage rs
  else pure unit

drawDeathMessage :: RendererState -> Effect Unit
drawDeathMessage rs = do
    drawText rs tileSize "You have" ( V{x:0.0, y:tileSize * 8.0} )
    drawText rs tileSize "died" ( V{x:1.5 * tileSize, y:tileSize * 9.0})

drawStats :: RendererState -> GameState -> Effect Unit
drawStats rs gs = do
  drawHp rs gs
  drawRedVision rs gs
  drawBlueVision rs gs

drawHp :: RendererState -> GameState -> Effect Unit
drawHp rs (GameState gs) = do
  let playerHp = (un Health gs.playerHealth).hpCount
  drawText rs tileSize (show playerHp) (V{ x:tileSize, y:0.0 })
  drawImage rs "heart.png" { x:0.0, y: 0.0, width: tileSize, height: tileSize }

drawRedVision :: RendererState -> GameState -> Effect Unit
drawRedVision rs (GameState gs) = do
  let playerBoard = (un Health gs.playerHealth).board
  if hasRedEye playerBoard
  then drawImage rs "Eye2.png" { x: tileSize * 2.0, y: 0.0, width: tileSize, height: tileSize}
  else pure unit

drawBlueVision :: RendererState -> GameState -> Effect Unit
drawBlueVision rs (GameState gs) = do
  let playerBoard = (un Health gs.playerHealth).board
  if hasBlueEye playerBoard
  then drawImage rs "Eye3.png" { x: tileSize * 4.0, y: 0.0, width: tileSize, height: tileSize}
  else pure unit

drawCenterPane :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawCenterPane
  t
  uis@(UIState {gsTimestamp})
  gs@(GameState {availableOrgans, level, installedOrgans, playerHealth})
  rs@(RendererState r) = do
  prevGS <- Ref.read r.gameStateId
  let gsDirty = maybe true ((>) $ gsTimestamp) (prevGS)
  case level of
       NewGame -> do
         clear rs centerPaneRect
         let x = centerPaneRect.x
             y = centerPaneRect.y + centerPaneRect.height - tileSize * 4.0
             size = tileSize / 2.0
             say s p = drawText rs size s p
         say "Press any key to start" (V{x,y})
         cacheScreen rs
       Victory -> do
         clear rs centerPaneRect
         let x = centerPaneRect.x
             y = centerPaneRect.y + centerPaneRect.height - tileSize * 8.0
             size = tileSize / 2.0
             say s p = drawText rs size s p
             (Board.Health h) = playerHealth
             (Board board) = h.board
             organs = RevMap.values board.organs
             freshOrgans = RevMap.values freshPlayerOrgans
             originalOrgansRemaining = Array.length
               $ Array.intersect organs freshOrgans
         say "You have escaped from the dump moon. Victory!" (V{x,y})
         say "But at what cost?" (V{x, y: y + tileSize * 2.0})
         say (String.replace (String.Pattern "{n}")
               (String.Replacement $ show installedOrgans)
               "You implanted {n} strange parts."
             )
             (V{x, y: y + tileSize * 3.0})
         say (String.replace (String.Pattern "{n}")
               (String.Replacement $ show originalOrgansRemaining)
               "You only have {n} original organs..." 
             )
             (V{x, y: y + tileSize * 4.0})
         cacheScreen rs
       Dead -> do
         clear rs centerPaneRect
         let x = centerPaneRect.x
             y = centerPaneRect.y + centerPaneRect.height - tileSize * 4.0
             size = tileSize / 2.0
             say s p = drawText rs size s p
         say "You died. Press any key to try again." (V{x,y})
         cacheScreen rs
       Regular _ -> do
         when gsDirty $ do
           centerPaneMap t uis gs rs
           cacheScreen rs
           Ref.write (Just gsTimestamp) r.gameStateId
         drawCenterPaneAnimations t uis gs rs
       Surgery _ -> do
         when gsDirty do
           clear rs centerPaneRect
           for_ (organArray availableOrgans) \(Tuple organ position) ->
             drawOrgan true rs organ (rectPos centerPaneRect + fromGrid position)
           let x = centerPaneRect.x
               y = centerPaneRect.y + centerPaneRect.height - tileSize * 4.0
               size = tileSize / 2.0
               say s p = drawText rs size s p
           say "Drag parts into your health area to install them" (V{x,y})
           say "Click on parts in your health to destroy them" (V{x,y: y + size})
           say "Press any key to go to the next level" (V{x,y: y + size * 2.0})
           cacheScreen rs
  Ref.write (Just gsTimestamp) r.gameStateId

centerPaneMap :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
centerPaneMap t (UIState uis) (GameState gs) rs = do
  clear rs centerPaneRect
  for_ gs.rooms \ {perimeter, visible} -> when visible $
    for_ (blockPositions $ perimeter) \pos ->
      let V p = pos
          terrain = fromMaybe Floor $ LI.index gs.terrain pos
          x = centerPaneRect.x + toNumber p.x * tileSize
          y = toNumber p.y * tileSize
          image = case terrain of
                   Wall -> "wall.png"
                   Floor -> "ground.png"
                   Exit -> "exit.png"
                   DoorClosed -> "doorclosed.png"
                   DoorOpen -> "dooropen.png"
      in drawImage rs image { x, y, width: tileSize, height: tileSize }

drawCenterPaneAnimations
  :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawCenterPaneAnimations
  t
  uis@(UIState {playerAnim})
  gs@(GameState g)
  r@(RendererState rs) = do
  drawImageTemp r "player.png" (animPlayerRect t uis gs)
  forWithIndex_ g.items \iid item@(Item{location}) ->
    let rect = animItemRect t iid item uis gs
        image = Item.image item
        (Item i) = item
        itemLifetime = i.decay
     in when (blink t itemLifetime) $ drawImageTemp r image rect
  forWithIndex_ g.enemies \eid nme ->
    let rect = animEnemyRect t eid nme uis gs
        image = Enemy.image nme
     in drawImageTemp r image rect

blink :: Instant -> Int -> Boolean
blink t f = Math.remainder ms interval > 100.0
  where
  ms = t # unInstant # un Milliseconds
  interval = 100.0 + (toNumber f * 100.0)

drawDraggedOrgan :: UIState -> GameState -> RendererState -> Effect Unit
drawDraggedOrgan (UIState{draggingOrgan}) gs rs@(RendererState r) = do
  case draggingOrgan of
       Nothing -> pure unit
       Just dragging@{organ: Tuple organ@(Organ (OrganSize w h) otype) p, offset } -> do
         let originalPos@(V{x,y}) = rectPos centerPaneRect + fromGrid p
             width = toNumber w * tileSize
             height = toNumber h * tileSize
         -- plaster over that organ's original position
         fillRectTemp rs {x,y,width,height} (Color "black")
         -- draw the moved organ
         let drawTo@(V dt) = originalPos + offset
             (V v) = offset
         drawOrganTemp true rs organ drawTo
         --TODO: surgery completion button

drawHighlights :: Instant -> UIState -> RendererState -> Effect Unit
drawHighlights t (UIState {highlights}) rs = do
  let color = getHighlightColor t
  for_ highlights \rect -> fillRectTemp rs rect color

getHighlightColor :: Instant -> Color
getHighlightColor t = Color $ "#ffffff" <> opacity
  where
  ms = t # unInstant # un Milliseconds
  opacity = sin (ms / 100.0) * 30.0 + 120.0
    # Int.floor
    # Int.toStringAs Int.hexadecimal

rectPos :: Rectangle -> Vector Number
rectPos {x,y} = V {x,y}

animPlayerRect :: Instant -> UIState -> GameState -> Rectangle
animPlayerRect t (UIState {playerAnim}) (GameState gs) =
  let V{x,y} = round <$>
               rectPos centerPaneRect
               + fromGrid gs.p
               + A.resolve t playerAnim
   in { x, y, width: tileSize, height: tileSize }

animEnemyRect :: Instant -> EnemyId -> Enemy -> UIState -> GameState -> Rectangle
animEnemyRect t eid (Enemy {location}) (UIState {enemyAnim}) (GameState gs) =
  let shift = fromMaybe zero $
              A.resolve t <$> Map.lookup eid enemyAnim
      V{x,y} = round <$>
                rectPos centerPaneRect
                + fromGrid location
                + shift
   in { x, y, width: tileSize, height: tileSize }

animItemRect :: Instant -> ItemId -> Item -> UIState -> GameState -> Rectangle
animItemRect t iid (Item {location}) (UIState {itemAnim}) (GameState gs) =
  let  V{x,y} = round <$>
                rectPos centerPaneRect
                + fromGrid location
   in { x, y, width: tileSize, height: tileSize }

drawRightPane :: Instant -> UIState -> GameState -> RendererState -> Effect Unit
drawRightPane t (UIState{rightPaneTarget,itemTarget}) g@(GameState gs) rs = do
  clear rs rightPaneRect
  case itemTarget of
    RPItem iid -> case Map.lookup iid gs.items of
      Nothing -> pure unit
      Just (Item i) -> do
        drawText rs tileSize "Item"
          (V { x: targetBoardContainerRect.x
          , y: targetBoardContainerRect.y + targetBoardContainerRect.height + tileSize * 2.0
          })
        drawText rs tileSize "turns"
          (V { x: targetBoardContainerRect.x
          , y: targetBoardContainerRect.y + targetBoardContainerRect.height + tileSize * 3.0
          })
        drawText rs tileSize "remaining:"
          (V { x: targetBoardContainerRect.x
          , y: targetBoardContainerRect.y + targetBoardContainerRect.height + tileSize * 4.0
          })
        drawText rs tileSize (show i.decay)
          (V { x: targetBoardContainerRect.x
          , y: targetBoardContainerRect.y + targetBoardContainerRect.height + tileSize * 5.0
          })
    _ -> pure unit
  case rightPaneTarget of
       RPEnemy eid -> case Map.lookup eid gs.enemies of
              Nothing -> pure unit
              Just n@(Enemy e) -> do
                let name = Enemy.name n
                    Health h = e.health
                    nameLoc = let {x,y} = rightPaneRect in V{x,y}
                wrapText rs name nameLoc rightPaneRect.width
                drawImage rs "heart.png"
                  { x: targetBoardContainerRect.x --TODO: move these outside
                  , y: targetBoardContainerRect.y -- for column clues
                  , width: tileSize, height: tileSize }
                drawText rs tileSize (show h.hpCount) -- TODO: move for column clues
                  (V { x: targetBoardContainerRect.x + tileSize
                  , y: targetBoardContainerRect.y
                  })
                drawImage rs "Armor5.png"
                  { x: targetBoardContainerRect.x + tileSize * 3.0
                  , y: targetBoardContainerRect.y
                  , width: tileSize, height: tileSize }
                drawText rs tileSize (show h.armorCount)
                  (V { x: targetBoardContainerRect.x + tileSize * 4.0
                  , y: targetBoardContainerRect.y
                  })
                drawBoardBase rs h.board targetBoardRect
                drawEnemyBoardDetails rs n g
       _ -> pure unit

clear :: RendererState -> Rectangle -> Effect Unit
clear rs rect = fillRect rs rect (Color "black")

fillRect :: RendererState -> Rectangle -> Color -> Effect Unit
fillRect (RendererState { cvars }) rect (Color c) = do
  ctx <- Canvas.getContext2D cvars.canvas
  Canvas.setFillStyle ctx c
  Canvas.fillRect ctx rect

drawBoardBase :: RendererState -> Board -> Rectangle -> Effect Unit
drawBoardBase vars (Board {injuries}) {x,y} =
  for_ (Array.range 0 5) \px ->
    for_ (Array.range 0 5) \py ->
      let buttonImage = if Set.member (V{x: px, y:py}) injuries
            then "ButtonPushed.png"
            else "ButtonUnpushed.png"
       in drawImage vars buttonImage
          { x: x + toNumber px * tileSize
          , y: y + toNumber py * tileSize
          , width: tileSize
          , height: tileSize
          }

drawEnemyBoardDetails :: RendererState -> Enemy -> GameState -> Effect Unit
drawEnemyBoardDetails rs (Enemy e) (GameState {playerHealth})= do
  let Health {board} = e.health
      anchor = let {x,y} = targetBoardRect in V{x,y}
  for_ (un Board board).injuries \v ->
    let drawPos = fromGrid v + anchor
     in case getOrganAtPosition board v of
          Just organ -> drawInjury rs organ drawPos
          Nothing -> case Map.lookup v e.clueCache of
                          Just clue -> drawClue
                                       rs
                                       (Board.restrictClue playerHealth clue)
                                       drawPos
                          Nothing -> pure unit

fromGrid :: Vector Int -> Vector Number
fromGrid p = p <#> \x -> toNumber x * tileSize

organImage :: OrganType -> Boolean -> String
organImage ot true = (Board.organImages ot).base
organImage ot false = (Board.organImages ot).hurt

drawOrgan :: Boolean -> RendererState -> Organ -> Vector Number -> Effect Unit
drawOrgan isIntact rs (Organ (OrganSize w h) organType) (V{x,y}) =
  drawImage rs (organImage organType isIntact) { x,y, width: tileSize * toNumber w, height: tileSize * toNumber h }

drawOrganTemp ::
  Boolean -> RendererState -> Organ -> Vector Number -> Effect Unit
drawOrganTemp isIntact rs (Organ (OrganSize w h) organType) (V{x,y}) =
    drawImageTemp rs (organImage organType isIntact)
      { x,y, width: tileSize * toNumber w, height: tileSize * toNumber h }

drawInjury :: RendererState -> Organ -> Vector Number -> Effect Unit
drawInjury = drawOrgan false

drawClue :: RendererState -> Clue -> Vector Number -> Effect Unit
drawClue rs (HpClue i) p =    drawColorText rs tileSize (show i) (Color "#a00") p
drawClue rs (ArmorClue i) p = drawColorText rs tileSize (show i) (Color "blue") p
drawClue rs (MixedClue i) p = drawColorText rs tileSize (show i) (Color "purple") p
drawClue rs EmptyClue p =     drawColorText rs tileSize "0" (Color "#777") p
drawClue rs ConcealedClue p = drawColorText rs tileSize "?" (Color "#333") p

drawText :: RendererState -> Number -> String -> Vector Number -> Effect Unit
drawText vars size string loc =
  drawColorText vars size string (Color "white") loc

newtype Color = Color String
foreign import setTextBaselineHanging :: Canvas.Context2D -> Effect Unit

drawColorText :: RendererState -> Number -> String -> Color -> Vector Number -> Effect Unit
drawColorText (RendererState {cvars}) size string (Color c) (V loc) = do
  ctx <- Canvas.getContext2D cvars.canvas
  Canvas.setFont ctx (show (Int.round size) <> "px CapitalHillMono")
  setTextBaselineHanging ctx
  Canvas.setFillStyle ctx c
  Canvas.fillText ctx string (loc.x + 5.0) (loc.y + 5.0)

wrapText
  :: RendererState
  -> String
  -> Vector Number
  -> Number
  -> Effect Unit
wrapText vars string (V loc) _maxWidth =
  let splits = splitOnSize string 11
      indices = scanl (\b a -> 1 + b) 0 splits
      splens = Array.zip indices splits
  in for_ splens \(Tuple i s) -> do
    drawText vars tileSize s (V loc {y = loc.y + tileSize * (toNumber $ i - 1)})

splitOnSize :: String -> Int -> Array String
splitOnSize s size =
  let splits = String.split (String.Pattern " ") s
      lens = scanl (\b a -> b + (String.length a)) 0 splits
      splens = Array.zip lens splits
      start =  String.joinWith " " $ map Tuple.snd (Array.takeWhile (\x -> Tuple.fst x < size) splens)
      rest = map Tuple.snd (Array.dropWhile (\x -> Tuple.fst x < size) splens)
      trail = String.joinWith " " rest
  in case Array.length rest > 0, String.length start > 0 of
       true, true -> [start] <> splitOnSize trail size
       _, true -> [start]
       _, false -> rest

{-
dirtyCheck :: Array Rectangle -> Rectangle -> Boolean
dirtyCheck dirty target = any (intersectRect target) dirty
  where
    intersectInterval l1 r1 l2 r2 = r1 > l2 && r2 > l1
    intersectRect r1 r2 =
      intersectInterval
        r1.x (r1.x + r1.width)
        r2.x (r2.x + r2.width)
      &&
      intersectInterval
        r1.y (r1.y + r1.height)
        r2.y (r2.y + r2.height)
-}

drawImage :: RendererState -> String -> Rectangle -> Effect Unit
drawImage r@(RendererState {cvars: { canvas, imageData }}) path {x,y, width, height} = do
  note r
  ctx <- Canvas.getContext2D canvas
  imageMap <- Ref.read imageData.index
  images <- Ref.read imageData.array
  let im = do
       (Image i) <- Map.lookup path imageMap
       Array.index images i
  case im of
       Nothing -> throwError (error "Missing image!")
       Just img -> Canvas.drawImageScale ctx img x y width height

drawImageFull :: RendererState -> String -> Rectangle -> Rectangle -> Effect Unit
drawImageFull r@(RendererState {cvars: { canvas, imageData }}) path s d = do
  note r
  ctx <- Canvas.getContext2D canvas
  imageMap <- Ref.read imageData.index
  images <- Ref.read imageData.array
  let im = do
       (Image i) <- Map.lookup path imageMap
       Array.index images i
  case im of
       Nothing -> throwError (error "Missing image!")
       Just img -> Canvas.drawImageFull ctx img
         s.x s.y s.width s.height
         d.x d.y d.width d.height
