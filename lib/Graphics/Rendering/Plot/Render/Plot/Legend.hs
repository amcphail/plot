-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Render.Plot.Legend
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

module Graphics.Rendering.Plot.Render.Plot.Legend (
                                       -- * Rendering
                                       renderLegend
                                       ) where

-----------------------------------------------------------------------------

import Data.Either
import Data.List(maximumBy)

import Data.Colour.Names

import qualified Data.Array.IArray as A
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Pango as P

import Control.Monad.Reader
import Control.Monad.State

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Defaults

import Graphics.Rendering.Plot.Figure.Text

import Graphics.Rendering.Plot.Render.Types
import Graphics.Rendering.Plot.Render.Text
import Graphics.Rendering.Plot.Render.Plot.Glyph

--import qualified Text.Printf as Printf

--import Prelude hiding(min,max)
--import qualified Prelude(max)

-----------------------------------------------------------------------------

renderLegend :: Maybe LegendData -> DataSeries -> Render (Padding -> Render ())
renderLegend Nothing                  _ = return $ \_ -> return ()
renderLegend (Just (Legend b l o to)) d = do
                                          -- calculate row height and max length
                                          let (ln,ls) = getLabels d
                                              mx = maximumBy (\ x y -> length x `compare` length y) $ fst $ unzip ls
                                          pc <- asks _pangocontext
                                          (w,h) <- cairo $ do
                                                           lo <- pango $ P.layoutText pc mx
                                                           setTextOptions to lo
                                                           (_,twh) <- textSize lo Centre Middle 0 0 
                                                           return twh
                                          -- if outside shift bounding box
                                          case o of
                                                 -- render legend
                                                 Outside -> do
                                                            outside <- renderLegendOutside b l w h to ln ls
                                                            return outside
                                                 -- else return (render legend)
                                                 Inside ->  return $ \_ -> renderLegendInside b l w h to ln ls

renderLegendOutside :: Bool -> LegendLocation -> Double -> Double -> TextOptions -> Int -> [(SeriesLabel,Decoration)] -> Render (Padding -> Render ())
renderLegendOutside b l w h to ln ls 
    | l == North                 = do
                                   let h' = textPad + h + textPad
                                   bbLowerTop $ h' + 4*textPad
                                   return $ \(Padding _ _ _ t) -> do
                                             x' <- bbCentreWidth
                                             y' <- bbTopHeight
                                             let w' = (fromIntegral ln)*(textPad + legendSampleWidth 
                                                      + legendSampleWidth + textPad + w) + 5*textPad
                                             let x = x'- (w'/2)
                                                 y = y'- h' - t
                                             when b (cairo $ renderBorder 1.0 black (x+0.5) (y+0.5) w' h')
                                             renderLegendEntries (x+3*textPad) (y+textPad) 
                                                                 (textPad + legendSampleWidth
                                                                  + legendSampleWidth + textPad 
                                                                  + w + textPad) 0 0 h to ls 
                                             return ()
    | l == NorthEast             = do
                                   let h' = textPad + h + textPad
                                   bbLowerTop $ h' + 4*textPad
                                   return $ \(Padding _ _ _ t) -> do
                                             x' <- bbRightWidth
                                             y' <- bbTopHeight
                                             let w' = (fromIntegral ln)*(textPad + legendSampleWidth 
                                                      + legendSampleWidth + textPad + w) + 5*textPad
                                             let x = x'- w'
                                                 y = y'- h' - t
                                             when b (cairo $ renderBorder 1.0 black (x+0.5) (y+0.5) w' h')
                                             renderLegendEntries (x+3*textPad) (y+textPad) 
                                                                 (textPad + legendSampleWidth
                                                                  + legendSampleWidth + textPad 
                                                                  + w + textPad) 0 0 h to ls 
                                             return ()
    | l == East                  = do
                                   let w' = textPad + legendSampleWidth 
                                            + legendSampleWidth + textPad + w + textPad
                                   bbShiftRight $ w' + 4*textPad
                                   return $ \(Padding _ r _ _) -> do
                                             x' <- bbRightWidth
                                             y' <- bbCentreHeight
                                             let h' = (fromIntegral ln)*(h+textPad) + 5*textPad
                                             let x = x' + 4*textPad + r
                                                 y = y'-(h'/2)
                                             when b (cairo $ renderBorder 1.0 black (x+0.5) (y+0.5) w' h')
                                             renderLegendEntries (x+2*textPad) (y+3*textPad) 0 (h+textPad) 0 h to ls 
                                             return ()
    | l == SouthEast             = do
                                   let h' = textPad + h + textPad
                                   bbRaiseBottom $ h' + 4*textPad
                                   return $ \(Padding _ _ b' _) -> do
                                             x' <- bbRightWidth
                                             y' <- bbBottomHeight
                                             let w' = (fromIntegral ln)*(textPad + legendSampleWidth 
                                                      + legendSampleWidth + textPad + w) + 5*textPad
                                             let x = x'- w'
                                                 y = y' + b' +textPad
                                             when b (cairo $ renderBorder 1.0 black (x+0.5) (y+0.5) w' h')
                                             renderLegendEntries (x+3*textPad) (y+textPad) 
                                                                 (textPad + legendSampleWidth
                                                                  + legendSampleWidth + textPad 
                                                                  + w + textPad) 0 0 h to ls 
                                             return ()
    | l == South                 = do
                                   let h' = textPad + h + textPad
                                   bbRaiseBottom $ h' + 4*textPad
                                   return $ \(Padding _ _ b' _) -> do
                                             x' <- bbCentreWidth
                                             y' <- bbBottomHeight
                                             let w' = (fromIntegral ln)*(textPad + legendSampleWidth 
                                                      + legendSampleWidth + textPad + w) + 5*textPad
                                             let x = x' - (w'/2)
                                                 y = y' + b' +textPad
                                             when b (cairo $ renderBorder 1.0 black (x+0.5) (y+0.5) w' h')
                                             renderLegendEntries (x+3*textPad) (y+textPad) 
                                                                 (textPad + legendSampleWidth
                                                                  + legendSampleWidth + textPad 
                                                                  + w + textPad) 0 0 h to ls 
                                             return ()
    | l == SouthWest             = do
                                   let h' = textPad + h + textPad
                                   bbRaiseBottom $ h' + 4*textPad
                                   return $ \(Padding _ _ b' _) -> do
                                             x' <- bbLeftWidth
                                             y' <- bbBottomHeight
                                             let w' = (fromIntegral ln)*(textPad + legendSampleWidth 
                                                      + legendSampleWidth + textPad + w) + 5*textPad
                                             let x = x'
                                                 y = y' + b' +textPad
                                             when b (cairo $ renderBorder 1.0 black (x+0.5) (y+0.5) w' h')
                                             renderLegendEntries (x+3*textPad) (y+textPad) 
                                                                 (textPad + legendSampleWidth
                                                                  + legendSampleWidth + textPad 
                                                                  + w + textPad) 0 0 h to ls 
                                             return ()
    | l == West                   = do
                                    let w' = textPad + legendSampleWidth 
                                             + legendSampleWidth + textPad + w + textPad
                                    bbShiftLeft $ w' + 4*textPad
                                    return $ \(Padding l _ _ _) -> do
                                             x' <- bbLeftWidth
                                             y' <- bbCentreHeight
                                             let h' = (fromIntegral ln)*(h+textPad) + 5*textPad
                                             let x = x' - w' - 4*textPad - l
                                                 y = y'-(h'/2)
                                             when b (cairo $ renderBorder 1.0 black (x+0.5) (y+0.5) w' h')
                                             renderLegendEntries (x+2*textPad) (y+3*textPad) 0 (h+textPad) 0 h to ls 
                                             return ()
    | l == NorthWest             = do
                                   let h' = textPad + h + textPad
                                   bbLowerTop $ h' + 4*textPad
                                   return $ \(Padding _ _ _ t) -> do
                                             x' <- bbLeftWidth
                                             y' <- bbTopHeight
                                             let w' = (fromIntegral ln)*(textPad + legendSampleWidth 
                                                      + legendSampleWidth + textPad + w) + 5*textPad
                                             let x = x'
                                                 y = y'- h' - t
                                             when b (cairo $ renderBorder 1.0 black (x+0.5) (y+0.5) w' h')
                                             renderLegendEntries (x+3*textPad) (y+textPad) 
                                                                 (textPad + legendSampleWidth
                                                                  + legendSampleWidth + textPad 
                                                                  + w + textPad) 0 0 h to ls 
                                             return ()

renderBorder :: Double -> Color -> Double -> Double -> Double -> Double -> C.Render ()
renderBorder lw c x y w h = do
                            C.setLineWidth lw
                            setColour c
                            C.rectangle x y w h
                            C.stroke
                            

renderLegendInside :: Bool -> LegendLocation -> Double -> Double -> TextOptions -> Int -> [(SeriesLabel,Decoration)] -> Render ()
renderLegendInside b l w h to ln ls = do
                                      let w' = (textPad + legendSampleWidth 
                                                + legendSampleWidth + textPad + w + textPad)
                                          h' = h+textPad
                                          h'' = (fromIntegral ln)*h'+textPad
                                      (x,y) <- case l of
                                                      North     -> do
                                                                   x' <- bbCentreWidth
                                                                   y' <- bbTopHeight
                                                                   return (x'-w'/2-textPad,y'+textPad)
                                                      NorthEast -> do
                                                                   x' <- bbRightWidth
                                                                   y' <- bbTopHeight
                                                                   return (x'-w'-3*textPad,y'+textPad)
                                                      East      -> do
                                                                   x' <- bbRightWidth
                                                                   y' <- bbCentreHeight
                                                                   let y'' = y' - h''/2 
                                                                   return (x'-w'-3*textPad,y''-textPad)
                                                      SouthEast -> do
                                                                   x' <- bbRightWidth
                                                                   y' <- bbBottomHeight
                                                                   let y'' = y' - h''
                                                                   return (x'-w'-3*textPad,y''-3*textPad)
                                                      South    -> do
                                                                   x' <- bbCentreWidth
                                                                   y' <- bbBottomHeight
                                                                   let y'' = y' - h''
                                                                   return (x'-w'/2-textPad,y''-3*textPad)
                                                      SouthWest -> do
                                                                   x' <- bbLeftWidth
                                                                   y' <- bbBottomHeight
                                                                   let y'' = y' - h''
                                                                   return (x'+textPad,y''-3*textPad)
                                                      West      -> do
                                                                   x' <- bbLeftWidth
                                                                   y' <- bbCentreHeight
                                                                   let y'' = y' - h''/2 
                                                                   return (x'+textPad,y''-textPad)
                                                      NorthWest -> do
                                                                   x' <- bbLeftWidth
                                                                   y' <- bbTopHeight
                                                                   return (x'+textPad,y'+textPad)
                                      when b (cairo $ renderBorder 1.0 black (x+0.5) (y+0.5) w' h'')
                                      cairo $ do
                                              --C.setSourceRGBA 1 0 0 0
                                              setColour white
                                              C.rectangle (x+0.5) (y+0.5) w' h''
                                              C.fill
                                              C.stroke
                                      renderLegendEntries (x+3*textPad) (y+textPad) 0 h' w' (h'-textPad) to ls 

renderLegendEntries :: Double -> Double -> Double -> Double -> Double -> Double 
                    -> TextOptions
                    -> [(SeriesLabel,Decoration)] -> Render ()
renderLegendEntries x y wa ha w h to ls = do
                              _ <- foldM (renderLegendEntry wa ha w h to) (x,y) ls
                              return ()

renderLegendEntry :: Double -> Double -> Double -> Double -> TextOptions -> (Double,Double) -> (SeriesLabel,Decoration) -> Render (Double,Double)
renderLegendEntry wa ha w h to (x,y) (l,d) = do
                            renderLegendSample x y legendSampleWidth h d
                            pc <- asks _pangocontext
                            cairo $ do
                                    lo <- pango $ P.layoutText pc l
                                    setTextOptions to lo
                                    showText lo (x+legendSampleWidth + 2*textPad) y
                            return (x+wa,y+ha)

renderLegendSample :: Double -> Double -> Double -> Double -> Decoration -> Render ()
renderLegendSample x y w h d = do
                               let l = decorationGetLineType d
                               let p = decorationGetPointType d
                               case l of
                                      Nothing -> return ()
                                      Just l' -> do
                                                 cairo $ do
                                                         setLineStyle l'
                                                         C.moveTo x     (y+h/2+0.5)
                                                         C.lineTo (x+w) (y+h/2+0.5)
                                                         C.stroke
                               case p of
                                      Nothing -> return ()
                                      Just p' -> do
                                                 cairo $ do
                                                         g <- setPointStyle p'
                                                         C.moveTo (x+w/2) (y+h/2)
                                                         renderGlyph 1 1 1 g

-----------------------------------------------------------------------------

getLabels :: DataSeries -> (Int,[(SeriesLabel,Decoration)])
getLabels (DS_Y d)      = let mls = map (\(DecSeries o d) -> (maybe "" id $ getOrdLabel o,d)) $ A.elems d
                              ln = length mls
                          in (ln,mls)
getLabels (DS_1toN _ d) = let mls = map (\(DecSeries o d) -> (maybe "" id $ getOrdLabel o,d)) $ A.elems d
                              ln = length mls
                          in (ln,mls)
getLabels (DS_1to1 d)   = let mls = map (\(_,(DecSeries o d)) -> (maybe "" id $ getOrdLabel o,d)) $ A.elems d
                              ln = length mls
                          in (ln,mls)
getLabels (DS_Surf _)   = (0,[])

-----------------------------------------------------------------------------

