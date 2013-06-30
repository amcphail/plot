{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- thanks to http://www.muitovar.com/gtk2hs/app1.html

--module Test where

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Monad.Trans

import Graphics.UI.Gtk hiding(Circle,Cross)
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Pango as P

import Data.Colour.Names

import Data.Packed.Vector
--import Data.Packed.Random
import Data.Packed()

--import Prelude.Unicode

import qualified Data.Array.IArray as A

import Numeric.LinearAlgebra

import Numeric.GSL.Statistics

import Graphics.Rendering.Plot

import Debug.Trace

ln = 25
ts = linspace ln (0,1)
rs = ln |> take ln [0.306399512330476,-0.4243863460546792,-0.20454667402138094,-0.42873761654774106,1.3054721019673694,0.6474765138733175,1.1942346875362946,-1.7404737823144103,0.2607101951530985,-0.26782584645524893,-0.31403631431884504,3.365508546473985e-2,0.6147856889630383,-1.191723225061435,-1.9933460981205509,0.6015225906539229,0.6394073044477114,-0.6030919788928317,0.1832742199706381,0.35532918011648473,0.1982646055874545,1.7928383756822786,-9.992760294442601e-2,-1.401166614128362,-1.1088031929569364,-0.827319908453775,1.0406363628775428,-0.3070345979284644,0.6781735212645198,-0.8431706723519456,-0.4245730055085966,-0.6503687925251668,-1.4775567962221399,0.5587634921497298,-0.6481020127107823,7.313441602898768e-2,0.573580543636529,-0.9036472376122673,2.650805059813826,9.329324044673039e-2,1.9133487025468563,-1.5366337588254542,-1.0159359710920388,7.95982933517428e-2,0.5813673663649735,-6.93329631989878e-2,1.1024137719307867,-0.6046286796589855,-0.8812842030098401,1.4612246471009083,0.9584060744500491,9.210899579679932e-2,-0.15850413664405813,-0.4754694827227343,0.8669922262489788,0.4593351854708853,-0.2015350278936992,0.8829710664887649,0.7195048491420026]

ut = linspace 25 (1::Double,100)

ss = sin (15*2*pi*ts)
ds = 0.25*rs + ss
es = constant (0.25*(stddev rs)) ln
gs = 0.40*rs - 1
fs :: Double -> Double
fs = sin . (15*2*pi*)

ms :: Matrix Double
ms = buildMatrix 64 64 (\(x,y) -> sin (2*2*pi*(fromIntegral x)/64) * cos (5*2*pi*(fromIntegral y)/64))

pts = linspace 1000 (0 :: Double,10*pi)
fx = (\t -> t * sin t) pts
fy = (\t -> t * cos t) pts

hx = fromList [1,3,5,8,11,20,22,26,12,10,4] :: Vector Double
hy = fromList [10,11,15,17,14,12,9,11,16,4,6] :: Vector Double
he = fromList [11,13,16,19,16,14,19,7,10,5,3] :: Vector Double

lx = fromList [1,2,3,4,5,6,7,8,9,10] ∷ Vector Double
ly = fromList [50000,10000,5000,1000,500,100,50,10,1] ∷ Vector Double

mx = linspace 100 (1,10) ∷ Vector Double
my = linspace 100 (1,10000) ∷ Vector Double

rx = scaleRecip 1 mx

cx = fromList [1,2,3,4,5] ∷ Vector Double
cyl = fromList [8,10,12,13,8] ∷ Vector Double
cyu = fromList [10,12,16,11,10] ∷ Vector Double
cel = cyl - 1
ceu = cyu + 1

at = linspace 1000 (0,2*pi) ∷ Vector Double
ax = sin at


figure = do
        withTextDefaults $ setFontFamily "OpenSymbol"
        withTitle $ setText "Multi-plot test"
--        setBackgroundColour yellow
        setPlots 4 2

        mapM_ (\(x,y) -> withPlot (x,y) $ do
                         setDataset (ts,[line ds blue])
--                         setPlotBackgroundColour grey
                         setPlotPadding 0 0 0 0
                         addAxis XAxis (Value 0) $ do
                           --  setGridlines Major True
                           setTicks Major (TickNumber 5)
                           setTicks Minor (TickNumber 41)
                         addAxis YAxis (Side Lower) $ do
                           setTicks Minor (TickNumber 0)
                         setRangeFromData YAxis Lower Linear
                         setRangeFromData XAxis Lower Linear) [(x,y)|x <- [1..4],y <- [1..2]]

display :: ((Int,Int) -> C.Render ()) -> IO ()
display r = do
   initGUI       -- is start

   window <- windowNew
   set window [ windowTitle := "Cairo test window"
              , windowDefaultWidth := 600
              , windowDefaultHeight := 400
              , containerBorderWidth := 1
              ]

--   canvas <- pixbufNew ColorspaceRgb True 8 300 200
--   containerAdd window canvas
   frame <- frameNew
   containerAdd window frame
   canvas <- drawingAreaNew
   containerAdd frame canvas
   widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

   widgetShowAll window 

   on canvas exposeEvent $ tryEvent $ do 
     s <- liftIO $ widgetGetSize canvas
     drw <- liftIO $ widgetGetDrawWindow canvas
     --dat <- liftIO $ takeMVar d
     --liftIO $ renderWithDrawable drw (circle 50 10)
     liftIO $ renderWithDrawable drw (r s)

   onDestroy window mainQuit
   mainGUI

          
main = display $ render figure

test = writeFigure PDF "test.pdf" (400,400) figure