-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Render.Text
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

module Graphics.Rendering.Plot.Render.Text (
                                       -- * Rendering
                                       renderText
                                       , renderTextVertical
                                       -- * Internal
                                       , textSize
                                       , textSizeVertical
                                       , showText
                                       , formatText
                                       ) where

-----------------------------------------------------------------------------

--import Data.Either

--import Data.Packed.Vector
--import Numeric.LinearAlgebra.Linear

--import Data.Word

--import Data.Maybe

--import Data.Colour.SRGB
--import Data.Colour.Names

---import qualified Data.Array.IArray as A

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Pango as P

import Control.Monad.Reader
--import Control.Monad.State
--import Control.Monad.Trans

import Graphics.Rendering.Plot.Types
--import Graphics.Rendering.Plot.Defaults

import Graphics.Rendering.Plot.Figure.Text

import Graphics.Rendering.Plot.Render.Types

--import qualified Text.Printf as Printf

--import Prelude hiding(min,max)
--import qualified Prelude(max)

-----------------------------------------------------------------------------

textSize :: P.PangoLayout -> TextXAlign -> TextYAlign -> Double -> Double -> C.Render ((Double,Double),(Double,Double))
textSize l xa ya x y = do
                       (_,P.PangoRectangle _ _ w h) <- pango $ P.layoutGetExtents l
                       return ((xStart xa x w h,yStart ya y h h),(w,h))
    where xStart TLeft   x' w' _  = x' - w'
          xStart Centre  x' w' _  = x' - (w'/2)
          xStart TRight  x' _  _  = x'
          yStart TBottom y' _  h' = y' - h'
          yStart Middle  y' _  h' = y' - (h'/2)
          yStart TTop    y' _  _  = y'

textSizeVertical :: P.PangoLayout -> TextXAlign -> TextYAlign -> Double -> Double -> C.Render ((Double,Double),(Double,Double))
textSizeVertical l xa ya x y = do
                       (_,P.PangoRectangle _ _ w h) <- pango $ P.layoutGetExtents l
                       return ((xStart xa x w h,yStart ya y w h),(w,h))
    where xStart TLeft   x' _  w' = x' - w'
          xStart Centre  x' _  w' = x' - (w'/2) 
          xStart TRight  x' _  _  = x'
          yStart TBottom y' _ _  = y'
          yStart Middle  y' h' _  = y' + (h'/2)
          yStart TTop    y' h' _  = y' + (h')

showText :: P.PangoLayout -> Double -> Double -> C.Render ()
showText pl x y = do
                  C.moveTo x y
                  P.showLayout pl

-----------------------------------------------------------------------------

formatText :: TextEntry -> Render TextEntry
formatText te@NoText          = return te
formatText (BareText s)       = do
                                to <- asks (_textoptions . _renderoptions)
                                return (FontText to s)
formatText (SizeText fz c s)  = do
                                to <- asks (_textoptions . _renderoptions)
                                return $ (FontText (changeFontSize fz $ changeFontColour c to) s)
formatText te@(FontText _ _)  = return te 

{-
getTextSize :: Text -> Render (Double,Double)
getTextSize (Text Nothing s) = do
                               to <- asks _text
                               getTextSize (Text to s)
getTextSize (Text (Just (TextOptions (FontOptions ff fs fw) fz _)) s) = cairo $ do
                               C.selectFontFace ff fs fw
                               C.setFontSize fz
                               te <- C.textExtents s
                               return (C.textExtentsWidth te,C.textExtentsHeight te)
-}
renderText :: TextEntry -> TextXAlign -> TextYAlign -> Double -> Double -> Render (Double,Double)
renderText NoText              _  _  _ _ = return (0,0)
renderText te@(BareText _)     xa ya x y = do
                                           te' <- formatText te
                                           renderText te' xa ya x y
renderText te@(SizeText _ _ _) xa ya x y = do
                                           te' <- formatText te
                                           renderText te' xa ya x y
renderText (FontText to s)     xa ya x y = do
                                           pc <- asks _pangocontext
                                           cairo $ do
                                                   lo <- pango $ P.layoutText pc s
                                                   setTextOptions to lo
                                                   ((x',y'),twh) <- textSize lo xa ya x y 
                                                   showText lo x' y'
                                                   return twh

renderTextVertical :: TextEntry -> TextXAlign -> TextYAlign -> Double -> Double -> Render (Double,Double)
renderTextVertical NoText            _  _  _ _ = return (0,0)
renderTextVertical (BareText s)      xa ya x y = do
                                         to <- asks (_textoptions . _renderoptions)
                                         renderTextVertical (FontText to s) xa ya x y
renderTextVertical (SizeText fz c s) xa ya x y = do
                                         to <- asks (_textoptions . _renderoptions)
                                         renderTextVertical (FontText (changeFontSize fz $
                                                                       changeFontColour c to) s) xa ya x y
renderTextVertical (FontText to s)   xa ya x y = do
                                         pc <- asks _pangocontext
                                         cairo $ do
                                                 lo <- pango $ P.layoutText pc s
                                                 setTextOptions to lo
                                                 C.moveTo x y
                                                 C.save
                                                 C.rotate (-pi/2)
                                                 --P.updateContext pc
                                                 P.updateLayout lo
                                                 ((x',y'),twh) <- textSizeVertical lo xa ya x y 
                                                 showText lo (-y') (-x')
                                                 C.restore
                                                 return twh
                                                      
-----------------------------------------------------------------------------
