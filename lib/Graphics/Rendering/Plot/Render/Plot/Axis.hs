-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Plot.Render.Plot.Axis
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

module Graphics.Rendering.Plot.Render.Plot.Axis (
                                       -- * Rendering
                                       renderAxes
                                       ) where

-----------------------------------------------------------------------------

import Data.Either

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Pango as P

import Control.Monad.Reader
import Control.Monad.State

import Graphics.Rendering.Plot.Types
import Graphics.Rendering.Plot.Defaults

import Graphics.Rendering.Plot.Figure.Text

import Graphics.Rendering.Plot.Render.Types
import Graphics.Rendering.Plot.Render.Text

import qualified Text.Printf as Printf

import Prelude hiding(min,max)
import qualified Prelude(max)

-----------------------------------------------------------------------------

addPadding :: Padding -> Padding -> Padding
addPadding (Padding l0 r0 b0 t0) (Padding l1 r1 b1 t1) = Padding (l0+l1) (r0+r1) (b0+b1) (t0+t1)

maxPadding :: Padding -> Padding -> Padding
maxPadding (Padding l0 r0 b0 t0) (Padding l1 r1 b1 t1) = Padding (Prelude.max l0 l1) (Prelude.max r0 r1) (Prelude.max b0 b1) (Prelude.max t0 t1)

-- first is plot padding, second is calculated padding
isZeroPadding :: Padding -> Padding -> Render Padding
isZeroPadding (Padding l0 r0 b0 t0) (Padding l1 r1 b1 t1) = do
                                                          l <- if l1 == 0 then do
                                                                               bbShiftLeft l0
                                                                               return l0
                                                                  else if l0 > l1 then do
                                                                                       bbShiftLeft (l0 - l1)
                                                                                       return l0
                                                                          else return l1
                                                          r <- if r1 == 0 then do
                                                                               bbShiftRight r0
                                                                               return r0
                                                                  else if r0 > r1 then do
                                                                                       bbShiftRight (r0 - r1)
                                                                                       return r0
                                                                          else return r1
                                                          b <- if b1 == 0 then do
                                                                               bbRaiseBottom b0
                                                                               return b0
                                                                  else if b0 > b1 then do
                                                                                       bbRaiseBottom (b0 - b1)
                                                                                       return b0
                                                                          else return b1
                                                          t <- if t1 == 0 then do
                                                                               bbLowerTop t0
                                                                               return t0
                                                                  else if t0 > t1 then do
                                                                                       bbLowerTop (t0 - t1)
                                                                                       return t0
                                                                          else return t1
                                                          return $ Padding l r b t


renderAxes :: Padding -> Ranges -> [AxisData] -> Render Padding
renderAxes p r axes = do
                      lp <- foldM shiftForAxisLabel (Padding 0 0 0 0) axes
                      tp <- foldM (shiftForTicks r) (Padding 0 0 0 0) axes
                      let apd = addPadding lp tp
                      p' <- isZeroPadding p apd
                      mapM_ (renderAxisLabel p') axes
                      mapM_ (renderAxis r) axes
                      return p'

shiftForAxisLabel :: Padding -> AxisData -> Render Padding
shiftForAxisLabel p (Axis _  _   _ _ _ _ NoText) = return p
shiftForAxisLabel p (Axis ax sd  _ _ _ _ lb) = do
                         (FontText to s) <- formatText lb
                         pc <- asks _pangocontext
                         (w,h) <- cairo $ do
                                          lo <- pango $ P.layoutText pc s
                                          setTextOptions to lo
                                          case ax of
                                                  XAxis -> do
                                                           (_,(twh)) <- textSize lo Centre Middle 0 0
                                                           return twh
                                                  YAxis -> do
                                                           (_,((w',h'))) <- textSizeVertical lo Centre Middle 0 0
                                                           return (h',w')
                         shiftForAxisLabel' p ax sd w h 
    where shiftForAxisLabel' (Padding l r b t) XAxis (Side Lower) _  h' = do
                  bbRaiseBottom (h'+2*textPad)
                  return $ Padding l r (b+h'+2*textPad) t
          shiftForAxisLabel' (Padding l r b t) XAxis (Side Upper) _  h' = do
                  bbLowerTop (h'+2*textPad)
                  return $ Padding l r b (t+h'+2*textPad)
          shiftForAxisLabel' (Padding l r b t) YAxis (Side Lower) w' _  = do
                  bbShiftLeft (w'+2*textPad)
                  return $ Padding (l+w'+2*textPad) r b t
          shiftForAxisLabel' (Padding l r b t) YAxis (Side Upper) w' _  = do
                  bbShiftRight (w'+2*textPad)
                  return $ Padding l (r+w'+2*textPad) b t
          shiftForAxisLabel' p'                _     (Value _)    _ _ = return p'

--    the padding is the tick padding that has been applied
renderAxisLabel :: Padding -> AxisData -> Render ()
renderAxisLabel _ (Axis _     _            _ _ _ _ NoText) = return ()
renderAxisLabel (Padding _ _ b _) (Axis XAxis (Side Lower) _ _ _ _ la) = do
                                       lx <- bbCentreWidth
                                       ly <- bbBottomHeight
                                       _ <- renderText la Centre TBottom lx (ly+b-textPad)
                                       return ()
renderAxisLabel (Padding _ _ _ t) (Axis XAxis (Side Upper) _ _ _ _ la) = do
                                       lx <- bbCentreWidth
                                       ly <- bbTopHeight
                                       _ <- renderText la Centre TTop lx (ly-t+textPad)
                                       return ()
renderAxisLabel (Padding l _ _ _) (Axis YAxis (Side Lower) _ _ _ _ la) = do
                                       lx <- bbLeftWidth
                                       ly <- bbCentreHeight
                                       _ <- renderTextVertical la TRight Middle (lx-l-2*textPad) ly
                                       return ()
renderAxisLabel (Padding _ r _ _) (Axis YAxis (Side Upper) _ _ _ _ la) = do
                                       lx <- bbRightWidth
                                       ly <- bbCentreHeight
                                       _ <- renderTextVertical la TLeft Middle (lx+r+2*textPad) ly
                                       return ()
renderAxisLabel _ (Axis _     (Value _)    _ _ _ _ _) = return ()

shiftForTicks :: Ranges -> Padding -> AxisData -> Render Padding
shiftForTicks (Ranges (Left (Range xmin xmax)) _)
                  p (Axis XAxis (Side Lower) _ min maj tf _) 
   = shiftForTicks' p min maj XAxis (Side Lower) tf (negate $ Prelude.max (abs xmin) (abs xmax))
shiftForTicks (Ranges (Left (Range xmin xmax)) _)
                  p (Axis XAxis (Side Upper) _ min maj tf _) 
   = shiftForTicks' p min maj XAxis (Side Upper) tf (negate $ Prelude.max (abs xmin) (abs xmax))
shiftForTicks (Ranges (Right ((Range xmin xmax),_)) _)
                  p (Axis XAxis (Side Lower) _ min maj tf _) 
   = shiftForTicks' p min maj XAxis (Side Lower) tf (negate $ Prelude.max (abs xmin) (abs xmax))
shiftForTicks (Ranges (Right (_,(Range xmin xmax))) _)
                  p (Axis XAxis (Side Upper) _ min maj tf _) 
   = shiftForTicks' p min maj XAxis (Side Upper) tf (negate $ Prelude.max (abs xmin) (abs xmax))
shiftForTicks (Ranges _ (Left (Range ymin ymax)))
                  p (Axis YAxis (Side Lower) _ min maj tf _) 
   = shiftForTicks' p min maj YAxis (Side Lower) tf (negate $ Prelude.max (abs ymin) (abs ymax))
shiftForTicks (Ranges _ (Left (Range ymin ymax)))
                  p (Axis YAxis (Side Upper) _ min maj tf _) 
   = shiftForTicks' p min maj YAxis (Side Upper) tf (negate $ Prelude.max (abs ymin) (abs ymax))
shiftForTicks (Ranges _ (Right ((Range ymin ymax),_)))
                  p (Axis YAxis (Side Lower) _ min maj tf _) 
   = shiftForTicks' p min maj YAxis (Side Lower) tf (negate $ Prelude.max (abs ymin) (abs ymax))
shiftForTicks (Ranges _ (Right (_,(Range ymin ymax))))
                  p (Axis YAxis (Side Upper) _ min maj tf _) 
   = shiftForTicks' p min maj YAxis (Side Upper) tf (negate $ Prelude.max (abs ymin) (abs ymax))
shiftForTicks _ p (Axis _ (Value _) _ _ _ _ _) 
   = return p

shiftForTicks' :: Padding -> Ticks -> Ticks -> AxisType -> AxisPosn -> TickFormat -> Double -> Render Padding
shiftForTicks' p (Ticks _ (Left 0)) (Ticks _ (Left 0)) _     _            _  _ = return p
shiftForTicks' p (Ticks _ (Left _)) (Ticks _ (Left 0)) _     _            _  _ = return p
{-
shiftForTicks' (Padding l r b t) (Ticks _ (Left _)) (Ticks _ (Left 0)) XAxis (Side Lower) _  _ = do
                 bbRaiseBottom minorTickLength
                 return $ Padding l r (b+minorTickLength) t
shiftForTicks' (Padding l r b t) (Ticks _ (Left _)) (Ticks _ (Left 0)) YAxis (Side Lower) _  _ = do
                 bbShiftLeft minorTickLength
                 return $ Padding (l+minorTickLength) r b t
shiftForTicks' (Padding l r b t) (Ticks _ (Left _)) (Ticks _ (Left 0)) XAxis (Side Upper) _  _ = do
                 bbLowerTop minorTickLength
                 return $ Padding l r b (t+minorTickLength)
shiftForTicks' (Padding l r b t) (Ticks _ (Left _)) (Ticks _ (Left 0)) YAxis (Side Upper) _  _ = do
                 bbShiftRight minorTickLength
                 return $ Padding l (r+minorTickLength) b t
-}
shiftForTicks' p                 (Ticks _ _)        (Ticks _ _)        ax    sd           tf v = do
                         to <- asks (_textoptions . _renderoptions)
                         pc <- asks _pangocontext
                         (tw,th) <- cairo $ do
                                           let s = Printf.printf tf v
                                           lt <- pango $ P.layoutText pc s
                                           setTextOptions (scaleFontSize tickLabelScale to) lt
                                           (_,twh) <- textSize lt Centre Middle 0 0
                                           return twh
                         shiftForTicks'' p (tw,th) ax sd
    where shiftForTicks'' (Padding l r b t) (_,th) XAxis (Side Lower) = do
               bbRaiseBottom (th+2*textPad)
               return $ Padding l r (b+th+2*textPad) t
          shiftForTicks'' (Padding l r b t) (tw,_) YAxis (Side Lower) = do
               bbShiftLeft (tw+2*textPad)
               return $ Padding (l+tw+2*textPad) r b t
          shiftForTicks'' (Padding l r b t) (_,th) XAxis (Side Upper) = do
               bbLowerTop (th+2*textPad)
               return $ Padding l r b (t+th+2*textPad)
          shiftForTicks'' (Padding l r b t) (tw,_) YAxis (Side Upper) = do
               bbShiftRight (tw+2*textPad)
               return $ Padding l (r+tw+2*textPad) b t
          shiftForTicks'' p' (_,_)  _     (Value _) = return p'

renderAxis :: Ranges -> AxisData -> Render ()
renderAxis _ (Axis _ _ NoLine _ _ _ _) = return () 
renderAxis r (Axis ax sd 
             (ColourLine c) 
             min maj tf l) = do
                             lo <- asks (_lineoptions . _renderoptions)
                             renderAxis r (Axis ax sd (TypeLine lo c) min maj tf l)
renderAxis r (Axis ax sd lt
             min maj tf _) = do
                             cairo $ setLineStyle lt
                             renderAxisLine r ax sd
                             cairo $ do
                                     lw' <- C.getLineWidth
                                     C.setLineWidth (lw'/2)
                             renderAxisTicks r ax sd min maj tf 
                             return ()

lowerRange :: Either Range (Range,Range) -> Range
lowerRange (Left r@(Range _ _))      = r
lowerRange (Right (r@(Range _ _),_)) = r

renderAxisLine :: Ranges -> AxisType -> AxisPosn -> Render ()
renderAxisLine (Ranges _ yr) XAxis (Value v) = do
                                    let (Range min max) = lowerRange yr                          
                                    (BoundingBox x y w h) <- get
                                    cairo $ do
                                            C.moveTo x     (y+h*((v-min)/(max-min))+0.5)
                                            C.lineTo (x+w) (y+h*((v-min)/(max-min))+0.5)
                                            C.stroke
renderAxisLine (Ranges xr _) YAxis (Value v) = do
                                    let (Range min max) = lowerRange xr                          
                                    (BoundingBox x y w h) <- get
                                    cairo $ do
                                            C.moveTo (x+w*((v-min)/(max-min))+0.5) y
                                            C.lineTo (x+w*((v-min)/(max-min))+0.5) (y+h)
                                            C.stroke
renderAxisLine _ XAxis (Side Lower) = do
                                    (BoundingBox x y w h) <- get
                                    cairo $ do
                                           C.moveTo x     (y+h+0.5)
                                           C.lineTo (x+w) (y+h+0.5)
                                           C.stroke
renderAxisLine _ XAxis (Side Upper) = do
                                    (BoundingBox x y w _) <- get
                                    cairo $ do
                                           C.moveTo x     (y+0.5)
                                           C.lineTo (x+w) (y+0.5)
                                           C.stroke
renderAxisLine _ YAxis (Side Lower) = do
                                    (BoundingBox x y _ h) <- get
                                    cairo $ do
                                           C.moveTo (x+0.5) y
                                           C.lineTo (x+0.5) (y+h)
                                           C.stroke
renderAxisLine _ YAxis (Side Upper) = do
                                    (BoundingBox x y w h) <- get
                                    cairo $ do
                                           C.moveTo (x+w+0.5) (y)
                                           C.lineTo (x+w+0.5) (y+h)
                                           C.stroke

tickPosition :: Double -> Double -> Int -> [Double]
tickPosition min max n = map (\x -> min + (max-min)*(fromIntegral x)/(fromIntegral (n-1))) (take n [(0 :: Int)..]) 
{-
tickPosition min max n = let diff = max - min
                             (sc,sd) = scaleDiff 1.0 diff n
                             start = (round (min*sc)) `div` (fromIntegral sd)
                         in map (\x -> (fromIntegral (x*sd+start))/sc) (take n [0..])
  where scaleDiff :: Double -> Double -> Int -> (Double,Int)
        scaleDiff s diff n
            | (round (s*diff)) < n = scaleDiff (10*s) (10*diff) n
            | otherwise            = (s,(round diff) `div` n)
-}
renderAxisTicks :: Ranges -> AxisType -> AxisPosn -> Ticks -> Ticks -> TickFormat -> Render ()
renderAxisTicks (Ranges xrange yrange) ax sd
                (Ticks gmin (Left tmin)) (Ticks gmaj (Left tmaj)) tf = do
       (BoundingBox x y w h) <- get
       to <- asks (_textoptions . _renderoptions)
       pc <- asks _pangocontext
       cairo $ do
              let (min,max) = case ax of
                   XAxis -> case sd of
                       (Side Lower) -> case xrange of
                             (Left (Range xmin xmax))    -> (xmin,xmax)
                             (Right (Range xmin xmax,_)) -> (xmin,xmax)
                       (Side Upper) -> case xrange of
                             (Left (Range xmin xmax))    -> (xmin,xmax)
                             (Right (_,Range xmin xmax)) -> (xmin,xmax)
                       (Value _)    -> case xrange of
                             (Left (Range xmin xmax))    -> (xmin,xmax)
                             (Right (Range xmin xmax,_)) -> (xmin,xmax) 
                   YAxis -> case sd of
                       (Side Lower) -> case yrange of
                             (Left (Range ymin ymax))    -> (ymin,ymax)
                             (Right (Range ymin ymax,_)) -> (ymin,ymax)
                       (Side Upper) -> case yrange of
                             (Left (Range ymin ymax))    -> (ymin,ymax)
                             (Right (_,Range ymin ymax)) -> (ymin,ymax)
                       (Value _)    -> case yrange of
                             (Left (Range ymin ymax))    -> (ymin,ymax)
                             (Right (Range ymin ymax,_)) -> (ymin,ymax)
              -- convert axis position to non-data coordinates
              let sd' = case sd of
                                (Side _)  -> sd
                                (Value v) -> case ax of
                                                     XAxis -> let (Range b t) = lowerRange yrange
                                                              in Value (y+h*(t-v)/(t-b))
                                                     YAxis -> let (Range b t) = lowerRange xrange
                                                              in Value (x+w*(v-b)/(t-b))
              let pos = (tickPosition min max tmaj)    
              let majpos = let ones = 1.0 : ones
                               ln = length pos
                               in zip pos (take ln ones)
                  minpos' = zip (tickPosition min max tmin) (minorTickLengths tmin tmaj)
                  minpos = filter (not . (\(p,_) -> elem p pos)) minpos' 
              let renderAxisTick' = renderAxisTick pc to x y w h min max ax sd' tf
              mapM_ (renderAxisTick' Minor gmin) minpos 
              mapM_ (renderAxisTick' Major gmaj) majpos
              return ()
       return ()

minorTickLengths :: Int -> Int -> [Double]
minorTickLengths min maj = let num = (min-1) `div` (maj-1)
                           in map ((/ 2) . (+ 1) . (* 2) . (/ (fromIntegral num)) . fromIntegral . (\x -> if x > (num `div` 2) then num - x else x) . (`mod` num)) (take (min+1) [0..])
                   --map ((/) 2 . (+) 1 . (/) (fromIntegral tmaj) . fromIntegral . (mod tmaj)) (take (tmin+1) [0..])

renderAxisTick :: P.PangoContext -> TextOptions 
               -> Double -> Double -> Double -> Double -> Double -> Double
               -> AxisType -> AxisPosn -> TickFormat -> Tick -> GridLines
               -> (Double,Double) -> C.Render ()
renderAxisTick pc to x y w h min max xa sd tf t gl (p,l) = do
       let tl' = case t of
                        Minor -> minorTickLength
                        Major -> majorTickLength
           tl = tl' * l
           (x1,y1,x2,y2) = case xa of
                                   XAxis -> case sd of
                                                    (Side Lower) -> let xt x' = x + (x'-min)*w/(max-min)
                                                                    in (xt p+0.5,y+h,xt p+0.5,y+h-tl)
                                                    (Side Upper) -> let xt x' = x + (x'-min)*w/(max-min)
                                                                    in (xt p+0.5,y,xt p+0.5,y+tl)
                                                    (Value v)    -> let xt x' = x + (x'-min)*w/(max-min)
                                                                    in (xt p+0.5,v-tl,xt p+0.5,v+tl)
                                   YAxis -> case sd of
                                                    (Side Lower) -> let yt y' = (y+h) - (y'-min)*h/(max-min)
                                                                    in (x,yt p+0.5,x+tl,yt p+0.5)
                                                    (Side Upper) -> let yt y' = (y+h) - (y'-min)*h/(max-min)
                                                                    in (x+w,yt p+0.5,x+w-tl,yt p+0.5)
                                                    (Value v)    -> let yt y' = (y + h) - (y'-min)*h/(max-min)
                                                                    in (v-tl,yt p+0.5,v+tl,yt p+0.5)
       C.moveTo x1 y1
       C.lineTo x2 y2
       C.stroke
       when gl (do
                let (x3,y3,x4,y4) = case xa of
                                   XAxis -> case sd of
                                                    (Side Lower) -> let xt x' = x + (x'-min)*w/(max-min)
                                                                    in (xt p+0.5,y,xt p+0.5,y+h)
                                                    (Side Upper) -> let xt x' = x + (x'-min)*w/(max-min)
                                                                    in (xt p+0.5,y,xt p+0.5,y+h)
                                                    (Value _)    -> let xt x' = x + (x'-min)*w/(max-min)
                                                                    in (xt p+0.5,y,xt p+0.5,y+h)
                                   YAxis -> case sd of
                                                    (Side Lower) -> let yt y' = (y+h) - (y'-min)*h/(max-min)
                                                                    in (x,yt p+0.5,x+w,yt p+0.5)
                                                    (Side Upper) -> let yt y' = (y+h) - (y'-min)*h/(max-min)
                                                                    in (x,yt p+0.5,x+w,yt p+0.5)
                                                    (Value _)    -> let yt y' = (y + h) - (y'-min)*h/(max-min)
                                                                    in (x,yt p+0.5,x+w,yt p+0.5)
                C.save
                C.setDash [2,2] 0
                C.setSourceRGBA 0 0 0 0.6
                C.moveTo x3 y3
                C.lineTo x4 y4
                C.stroke
                C.restore)
       let majlab = case sd of 
                            (Side _)  -> True
                            (Value _) -> False
       when (t == Major && majlab) $ do
            let s = Printf.printf tf p
            lo <- pango $ P.layoutText pc s
            setTextOptions (scaleFontSize tickLabelScale to) lo
            case xa of 
                    XAxis -> do
                             case sd of
                                     (Side Lower) -> do
                                                     ((x',y'),_) <- textSize lo Centre TTop x1 (y1+2*textPad)
                                                     showText lo x' y'
                                     (Side Upper) -> do
                                                     ((x',y'),_) <- textSize lo Centre TBottom x1 (y1-2*textPad)
                                                     showText lo x' y'
                                     (Value _)    -> error "renderAxisTicks: unreachable code (when majlab)"
                    YAxis -> do
                             case sd of
                                     (Side Lower) -> do
                                                     ((x',y'),_) <- textSize lo TLeft Middle (x1-2*textPad) y1
                                                     showText lo x' y'
                                     (Side Upper) -> do
                                                     ((x',y'),_) <- textSize lo TRight Middle (x1+2*textPad) y1
                                                     showText lo x' y'
                                     (Value _)    -> error "renderAxisTicks: unreachable code (when majlab)"
            return ()

-----------------------------------------------------------------------------

