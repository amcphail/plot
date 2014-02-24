import Graphics.Rendering.Plot
import Numeric.LinearAlgebra
import Numeric.GSL.Statistics
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Pango.Enums
import Graphics.UI.Gtk hiding(Circle,Cross) 
import Control.Monad.Trans

ln = 25
ts = linspace ln (0,1)
rs = randomVector 0 Gaussian ln

ss = sin (15*2*pi*ts)
ds = 0.25*rs + ss
es = constant (0.25*(stddev rs)) ln

fs :: Double -> Double
fs = sin . (15*2*pi*)

test_graph = do
       withTitle $ setText "Testing plot package:"
       withSubTitle $ do
                      setText "with 1 second of a 15Hz sine wave"
       withPointDefaults $ setPointSize 1.5 -- (A)
       setPlots 1 1
       withPlot (1,1) $ do
                        setLegend True SouthWest Inside -- (B)
                        addAxis XAxis (Side Lower) $ withAxisLabel $ setText "time (s)"
                        addAxis YAxis (Side Lower) $ do
                          withAxisLabel $ setText "amplitude"
                          withGridLine Major $ do
                            setDashStyle [Dash]
                            setLineColour lightgray
                        addAxis XAxis (Value 0) $ return ()
                        setRange YAxis Lower Linear (-1.25) 1.25
                        setDataset (ts,[point (ds,es,"dat") (Cross,red),line (fs,"sin") blue])
                        setRangeFromData XAxis Lower Linear

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

   on canvas exposeEvent $ tryEvent $ do s <- liftIO $ widgetGetSize canvas
                                         drw <- liftIO $ widgetGetDrawWindow canvas
                                         --dat <- liftIO $ takeMVar d
                                         --liftIO $ renderWithDrawable drw (circle 50 10)
                                         liftIO $ renderWithDrawable drw (r s)

   onDestroy window mainQuit
   mainGUI

          
main = display $ render test_graph

test_render :: (Int,Int) -> C.Render ()
test_render = render test_graph

--main = C.withSVGSurface "out.svg" 400 400 $ \surf -> C.renderWith surf $ test_render (400,400)