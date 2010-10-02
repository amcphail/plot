-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Render.Plot.Glyph
-- Copyright   :  (c) A. V. H. McPhail 2010
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Rendering 'Figure's
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Plot.Render.Plot.Glyph (
                                       -- * Rendering
                                       renderGlyph
                                       ) where

-----------------------------------------------------------------------------

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Plot.Types

-----------------------------------------------------------------------------

glyphWidth :: Double
glyphWidth = 2*pi

renderGlyph :: Double -> Double -> LineWidth -> Glyph -> C.Render ()
renderGlyph xscale yscale pz g = do
                                 C.save
                                 C.scale (pz / xscale) (pz / yscale)
                                 C.setLineWidth 1
                                 renderGlyph' g
                                 C.restore
   where renderGlyph' Box    = renderGlyphBox 
         renderGlyph' Cross  = renderGlyphCross 
         renderGlyph' Diamond = renderGlyphDiamond 
         renderGlyph' Asterisk = renderGlyphAsterisk 
         renderGlyph' Triangle = renderGlyphTriangle 
         renderGlyph' Circle = renderGlyphCircle 
         renderGlyph' Bullet = renderGlyphBullet 
         renderGlyph' Top    = renderGlyphTop    
         renderGlyph' Bot    = renderGlyphBot   
--renderGlyph _      _      _      = return ()

difference :: Num a => [a] -> [a]
difference [] = []
difference [_] = []
difference (x0:x1:xs) = (x1-x0):(difference (x1:xs))

renderGlyphBox :: C.Render ()
renderGlyphBox = do
                 let x = glyphWidth
                     y = glyphWidth
                 C.relMoveTo (-x/2) (-y/2)
                 C.relLineTo 0 y
                 C.relLineTo x 0
                 C.relLineTo 0 (-y)
                 C.closePath
                 C.strokePreserve
                 C.save
                 C.setSourceRGBA 1 1 1 1
                 C.fill
                 C.restore

renderGlyphCross :: C.Render ()
renderGlyphCross = do
                   let x = glyphWidth
                       y = glyphWidth
                   C.relMoveTo (-x/2) 0
                   C.relLineTo x 0
                   C.relMoveTo (-x/2) (-y/2)
                   C.relLineTo 0 y
                   C.closePath
                   C.stroke

renderGlyphDiamond :: C.Render ()
renderGlyphDiamond = do
                     let x = glyphWidth
                         y = glyphWidth
                     C.relMoveTo (-x/2) 0
                     C.relLineTo (x/2) y
                     C.relLineTo (x/2) (-y)
                     C.relLineTo (-x/2) (-y)
                     C.closePath
                     C.strokePreserve
                     C.save
                     C.setSourceRGBA 1 1 1 1
                     C.fill
                     C.restore
                     C.stroke

renderGlyphAsterisk :: C.Render ()
renderGlyphAsterisk = do
                      let radius = glyphWidth/2
                          angles' = map ((+ 90) . (* (360 `div` 5))) [0..4]
                          angles = map ((* (2*pi/360)) . fromInteger . (`mod` 360)) angles'
                          xs = map ((* (radius)) . cos) angles
                          ys = map ((* (radius)) . sin) angles
                      mapM_ (\(x,y) -> do
                                       C.relLineTo x y
                                       C.relMoveTo (-x) (-y)) (zip xs ys)
                      C.stroke

renderGlyphTriangle :: C.Render ()
renderGlyphTriangle = do
                      let radius = glyphWidth/2
                          angles' = [90,210,330]
                          --angles' = map ((flip (+) 90) . (* (360 `div` 3))) [0..2]
                          angles = map ((* (2*pi/360)) . fromInteger . (`mod` 360)) angles'
                          x@(sx:_) = map ((* (radius)) . cos) angles
                          y@(sy:_) = map ((* (radius)) . sin) angles
                          xs = difference x
                          ys = difference y
                      C.relMoveTo sx sy
                      mapM_ (uncurry C.relLineTo) (zip xs ys)
                      C.closePath
                      C.strokePreserve
                      C.save
                      C.setSourceRGBA 1 1 1 1
                      C.fill
                      C.restore

renderGlyphCircle :: C.Render ()
renderGlyphCircle = do
                    let radius = glyphWidth/2
                        angles = map (*(2*pi/36)) [0..35] 
                        x@(sx:_) = map ((* (radius)) . cos) angles
                        y@(sy:_) = map ((* (radius)) . sin) angles
                        xs = difference x
                        ys = difference y
                    C.relMoveTo sx sy
                    mapM_ (uncurry C.relLineTo) (zip xs ys)
                    C.closePath
                    C.strokePreserve
                    C.save
                    C.setSourceRGBA 1 1 1 1
                    C.fill
                    C.restore
                        
renderGlyphBullet :: C.Render ()
renderGlyphBullet = do
                    let radius = glyphWidth/2
                        angles = map (*(2*pi/36)) [0..35] 
                        x@(sx:_) = map ((* (radius)) . cos) angles
                        y@(sy:_) = map ((* (radius)) . sin) angles
                        xs = difference x
                        ys = difference y
                    C.relMoveTo sx sy
                    mapM_ (uncurry C.relLineTo) (zip xs ys)
                    C.closePath
                    C.fill
                    C.stroke
                        
renderGlyphTop :: C.Render ()
renderGlyphTop = do
                 let x = glyphWidth
                     y = glyphWidth
                 C.relMoveTo (-x/2) 0
                 C.relLineTo x 0
                 C.relMoveTo (-x/2) 0
                 C.relLineTo 0 (-y)
                 C.stroke

renderGlyphBot :: C.Render ()
renderGlyphBot = do
                 let x = glyphWidth
                     y = glyphWidth
                 C.relMoveTo (-x/2) 0
                 C.relLineTo x 0
                 C.relMoveTo (-x/2) 0
                 C.relLineTo 0 (y)
                 C.stroke

-----------------------------------------------------------------------------


