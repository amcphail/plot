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

--import Data.Eq.Unicode
--import Data.Function.Unicode
--import Prelude.Unicode

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

moveTo :: Double -> Double -> C.Render ()
moveTo x y = do
  lw <- C.getLineWidth
  let (x',y') = if lw >= 1 && (round lw `mod` 2 == (0 :: Int))
                then (fromIntegral $ (round x :: Int),fromIntegral $ (round y :: Int))
                else ((((fromIntegral $ (round x :: Int))*2)+1)/2,(((fromIntegral $ (round y :: Int))*2)+1)/2)
  C.moveTo x' y'

lineTo :: Double -> Double -> C.Render ()
lineTo x y = do
  lw <- C.getLineWidth
  let (x',y') = if lw >= 1 && (round lw `mod` 2 == (0 :: Int))
                then (fromIntegral $ (round x :: Int),fromIntegral $ (round y :: Int))
                else ((((fromIntegral $ (round x :: Int))*2)+1)/2,(((fromIntegral $ (round y :: Int))*2)+1)/2)
  C.lineTo x' y'

-----------------------------------------------------------------------------

addPadding :: Padding -> Padding -> Padding
addPadding (Padding l0 r0 b0 t0) (Padding l1 r1 b1 t1) = Padding (l0+l1) (r0+r1) (b0+b1) (t0+t1)

--maxPadding :: Padding -> Padding -> Padding
--maxPadding (Padding l0 r0 b0 t0) (Padding l1 r1 b1 t1) = Padding (Prelude.max l0 l1) (Prelude.max r0 r1) (Prelude.max b0 b1) (Prelude.max t0 t1)

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
shiftForAxisLabel p (Axis _  _   _ _ _ _ _ NoText) = return p
shiftForAxisLabel p (Axis ax sd  _ _ _ _ _ lb) = do
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
renderAxisLabel _ (Axis _     _            _ _ _ _ _ NoText) = return ()
renderAxisLabel (Padding _ _ b _) (Axis XAxis (Side Lower) _ _ _ _ _ la) = do
  lx <- bbCentreWidth
  ly <- bbBottomHeight
  _ <- renderText la Centre TBottom lx (ly+b-textPad)
  return ()
renderAxisLabel (Padding _ _ _ t) (Axis XAxis (Side Upper) _ _ _ _ _ la) = do
  lx <- bbCentreWidth
  ly <- bbTopHeight
  _ <- renderText la Centre TTop lx (ly-t+textPad)
  return ()
renderAxisLabel (Padding l _ _ _) (Axis YAxis (Side Lower) _ _ _ _ _ la) = do
  lx <- bbLeftWidth
  ly <- bbCentreHeight
  _ <- renderTextVertical la TRight Middle (lx-l-2*textPad) ly
  return ()
renderAxisLabel (Padding _ r _ _) (Axis YAxis (Side Upper) _ _ _ _ _ la) = do
  lx <- bbRightWidth
  ly <- bbCentreHeight
  _ <- renderTextVertical la TLeft Middle (lx+r+2*textPad) ly
  return ()
renderAxisLabel _ (Axis _     (Value _)    _ _ _ _ _ _ ) = return ()

shiftForTicks :: Ranges -> Padding -> AxisData -> Render Padding
shiftForTicks (Ranges (Left (Range _ xmin xmax)) _)
                  p (Axis XAxis (Side Lower) _ min maj tf dl _) 
   = shiftForTicks' p min maj XAxis (Side Lower) tf dl (negate $ Prelude.max (abs xmin) (abs xmax))
shiftForTicks (Ranges (Left (Range _ xmin xmax)) _)
                  p (Axis XAxis (Side Upper) _ min maj tf dl _) 
   = shiftForTicks' p min maj XAxis (Side Upper) tf dl (negate $ Prelude.max (abs xmin) (abs xmax))
shiftForTicks (Ranges (Right ((Range _ xmin xmax),_)) _)
                  p (Axis XAxis (Side Lower) _ min maj tf dl _) 
   = shiftForTicks' p min maj XAxis (Side Lower) tf dl (negate $ Prelude.max (abs xmin) (abs xmax))
shiftForTicks (Ranges (Right (_,(Range _ xmin xmax))) _)
                  p (Axis XAxis (Side Upper) _ min maj tf dl _) 
   = shiftForTicks' p min maj XAxis (Side Upper) tf dl (negate $ Prelude.max (abs xmin) (abs xmax))
shiftForTicks (Ranges _ (Left (Range _ ymin ymax)))
                  p (Axis YAxis (Side Lower) _ min maj tf dl _) 
   = shiftForTicks' p min maj YAxis (Side Lower) tf dl (negate $ Prelude.max (abs ymin) (abs ymax))
shiftForTicks (Ranges _ (Left (Range _ ymin ymax)))
                  p (Axis YAxis (Side Upper) _ min maj tf dl _) 
   = shiftForTicks' p min maj YAxis (Side Upper) tf dl (negate $ Prelude.max (abs ymin) (abs ymax))
shiftForTicks (Ranges _ (Right ((Range _ ymin ymax),_)))
                  p (Axis YAxis (Side Lower) _ min maj tf dl _) 
   = shiftForTicks' p min maj YAxis (Side Lower) tf dl (negate $ Prelude.max (abs ymin) (abs ymax))
shiftForTicks (Ranges _ (Right (_,(Range _ ymin ymax))))
                  p (Axis YAxis (Side Upper) _ min maj tf dl _) 
   = shiftForTicks' p min maj YAxis (Side Upper) tf dl (negate $ Prelude.max (abs ymin) (abs ymax))
shiftForTicks _ p (Axis _ (Value _) _ _ _ _ _ _) 
   = return p

shiftForTicks' :: Padding -> Maybe Ticks -> Maybe Ticks -> AxisType -> AxisPosn -> TickFormat -> [TextEntry] -> Double -> Render Padding
shiftForTicks' p Nothing            Nothing            _     _            _  _  _ = return p
shiftForTicks' p (Just (Ticks _ _)) Nothing            _     _            _  _  _ = return p
shiftForTicks' p _                  (Just (Ticks _ _)) ax    sd           tf dl v = do
  to <- asks (_textoptions . _renderoptions)
  pc <- asks _pangocontext
  (tw,th) <- cairo $ do
     let s = formatTick tf v
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

renderAxis :: Ranges -> AxisData -> Render ()
renderAxis _ (Axis _ _ NoLine _ _ _ _ _) = return () 
renderAxis r (Axis ax sd 
             (ColourLine c) 
             min maj tf dl l) = do
  lo <- asks (_lineoptions . _renderoptions)
  renderAxis r (Axis ax sd (TypeLine lo c) min maj tf dl l)
renderAxis r (Axis ax sd lt
             min maj tf dl _) = do
  cairo $ setLineStyle lt
  renderAxisTicks r ax sd min maj tf dl
  renderAxisLine r ax sd
  return ()

lowerRange :: Either Range (Range,Range) -> Range
lowerRange (Left r@(Range _ _ _))      = r
lowerRange (Right (r@(Range _ _ _),_)) = r

renderAxisLine :: Ranges -> AxisType -> AxisPosn -> Render ()
renderAxisLine (Ranges _ yr) XAxis (Value v) = do
  let (Range _ min max) = lowerRange yr                          
  (BoundingBox x y w h) <- get
  cairo $ do
    lw' <- C.getLineWidth
    let lw = lw'/2
    moveTo (x-lw)   (y+h*((max-v)/(max-min)))
    lineTo (x+w+lw) (y+h*((max-v)/(max-min)))
    C.stroke
renderAxisLine (Ranges xr _) YAxis (Value v) = do
  let (Range _ min max) = lowerRange xr                          
  (BoundingBox x y w h) <- get
  cairo $ do
    lw' <- C.getLineWidth
    let lw = lw'/2
    moveTo (x+w*((v-min)/(max-min))) (y-lw)
    lineTo (x+w*((v-min)/(max-min))) (y+h+lw)
    C.stroke
renderAxisLine _ XAxis (Side Lower) = do
  (BoundingBox x y w h) <- get
  cairo $ do
    lw' <- C.getLineWidth
    let lw = lw'/2
    moveTo (x-lw)   (y+h)
    lineTo (x+w+lw) (y+h)
    C.stroke
renderAxisLine _ XAxis (Side Upper) = do
  (BoundingBox x y w _) <- get
  cairo $ do
    lw' <- C.getLineWidth
    let lw = lw'/2
    moveTo (x-lw)   (y)
    lineTo (x+w+lw) (y)
    C.stroke
renderAxisLine _ YAxis (Side Lower) = do
  (BoundingBox x y _ h) <- get
  cairo $ do
    lw' <- C.getLineWidth
    let lw = lw'/2
    moveTo (x) (y-lw)
    lineTo (x) (y+h+lw)
    C.stroke
renderAxisLine _ YAxis (Side Upper) = do
  (BoundingBox x y w h) <- get
  cairo $ do
    lw' <- C.getLineWidth
    let lw = lw'/2
    moveTo (x+w) (y-lw)
    lineTo (x+w) (y+h+lw)
    C.stroke

tickPosition :: Scale -> Double -> Double -> Int -> [(Double,Double)]
tickPosition sc min max n = 
  let pos = map (\x -> min + (max-min)*(x)/(fromIntegral (n-1))) (take n [(0 :: Double)..])
      val = if sc == Log
            then map (\x -> logBase 10 min + (x/(fromIntegral (n-1))) * (logBase 10 max - logBase 10 min))
              (take n [(0 :: Double)..])
            else pos
  in zip pos val
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
renderAxisTicks :: Ranges -> AxisType -> AxisPosn -> Maybe Ticks -> Maybe Ticks -> TickFormat -> [TextEntry] -> Render ()
renderAxisTicks (Ranges xrange yrange) ax sd
                (Just (Ticks gmin (TickNumber tmin))) (Just (Ticks gmaj (TickNumber tmaj))) tf dl = do
       (BoundingBox x y w h) <- get
       to <- asks (_textoptions . _renderoptions)
       pc <- asks _pangocontext
       cairo $ do
              let (sc,min,max) = case ax of
                   XAxis -> case sd of
                       (Side Lower) -> case xrange of
                             (Left (Range scl xmin xmax))    -> (scl,xmin,xmax)
                             (Right (Range scl xmin xmax,_)) -> (scl,xmin,xmax)
                       (Side Upper) -> case xrange of
                             (Left (Range scl xmin xmax))    -> (scl,xmin,xmax)
                             (Right (_,Range scl xmin xmax)) -> (scl,xmin,xmax)
                       (Value _)    -> case xrange of
                             (Left (Range scl xmin xmax))    -> (scl,xmin,xmax)
                             (Right (Range scl xmin xmax,_)) -> (scl,xmin,xmax) 
                   YAxis -> case sd of
                       (Side Lower) -> case yrange of
                             (Left (Range scl ymin ymax))    -> (scl,ymin,ymax)
                             (Right (Range scl ymin ymax,_)) -> (scl,ymin,ymax)
                       (Side Upper) -> case yrange of
                             (Left (Range scl ymin ymax))    -> (scl,ymin,ymax)
                             (Right (_,Range scl ymin ymax)) -> (scl,ymin,ymax)
                       (Value _)    -> case yrange of
                             (Left (Range scl ymin ymax))    -> (scl,ymin,ymax)
                             (Right (Range scl ymin ymax,_)) -> (scl,ymin,ymax)
              -- convert axis position to non-data coordinates
              let sd' = case sd of
                          (Side _)  -> sd
                          (Value v) -> case ax of
                                        XAxis -> let (Range _ b t) = lowerRange yrange
                                                in Value (y+h*(t-v)/(t-b))
                                        YAxis -> let (Range _ b t) = lowerRange xrange
                                                in Value (x+w*(v-b)/(t-b))
              let (pos,val) = unzip (tickPosition sc min max tmaj)    
              let majpos = let ones = 1.0 : ones
                               ln = length pos
                               in zip3 pos (take ln ones) val
                  (pos',val') = unzip (tickPosition sc min max tmin)
                  minpos' = zip3 pos' (minorTickLengths tmin tmaj) val'
                  minpos = filter (not . (\(p,_,_) -> elem p pos)) minpos' 
              let renderAxisTick' = renderAxisTick pc to x y w h sc min max ax sd' tf
              mapM_ (renderAxisTick' Minor gmin) minpos 
              mapM_ (renderAxisTick' Major gmaj) majpos
              return ()
       return ()

minorTickLengths :: Int -> Int -> [Double]
minorTickLengths min maj = let num = (min-1) `div` (maj-1)
                           in map ((/ 2) . (+ 1) . (* 2) . (/ (fromIntegral num)) . fromIntegral . (\x -> if x > (num `div` 2) then num - x else x) . (`mod` num)) (take (min+1) [0..])
                   --map ((/) 2 . (+) 1 . (/) (fromIntegral tmaj) . fromIntegral . (mod tmaj)) (take (tmin+1) [0..])

renderAxisTick :: P.PangoContext -> TextOptions 
               -> Double -> Double -> Double -> Double -> Scale -> Double -> Double
               -> AxisType -> AxisPosn -> TickFormat -> Tick -> LineType
               -> (Double,Double,Double) -> C.Render ()
renderAxisTick pc to x y w h sc min max xa sd tf t gl (p,l,v) = do
       let tl' = case t of
                        Minor -> minorTickLength
                        Major -> majorTickLength
           tl = tl' * l
           (x1,y1,x2,y2) = case xa of
                             XAxis -> case sd of
                               (Side Lower) -> let xt x' = x + (x'-min)*w/(max-min)
                                              in (xt p,y+h,xt p,y+h-tl)
                               (Side Upper) -> let xt x' = x + (x'-min)*w/(max-min)
                                              in (xt p,y,xt p,y+tl)
                               (Value v')   -> let xt x' = x + (x'-min)*w/(max-min)
                                              in (xt p,v'-tl,xt p,v'+tl)
                             YAxis -> case sd of
                               (Side Lower) -> let yt y' = (y+h) - (y'-min)*h/(max-min)
                                              in (x,yt p,x+tl,yt p)
                               (Side Upper) -> let yt y' = (y+h) - (y'-min)*h/(max-min)
                                              in (x+w,yt p,x+w-tl,yt p)
                               (Value v')   -> let yt y' = (y + h) - (y'-min)*h/(max-min)
                                              in (v'-tl,yt p,v'+tl,yt p)
       moveTo x1 y1
       lineTo x2 y2
       C.stroke
       when (gl /= NoLine)  (do
         let (x3,y3,x4,y4) = case xa of
                               XAxis -> case sd of
                                         (Side Lower) -> let xt x' = x + (x'-min)*w/(max-min)
                                                        in (xt p,y,xt p,y+h)
                                         (Side Upper) -> let xt x' = x + (x'-min)*w/(max-min)
                                                        in (xt p,y,xt p,y+h)
                                         (Value _)    -> let xt x' = x + (x'-min)*w/(max-min)
                                                        in (xt p,y,xt p,y+h)
                               YAxis -> case sd of
                                         (Side Lower) -> let yt y' = (y+h) - (y'-min)*h/(max-min)
                                                        in (x,yt p,x+w,yt p)
                                         (Side Upper) -> let yt y' = (y+h) - (y'-min)*h/(max-min)
                                                        in (x,yt p,x+w,yt p)
                                         (Value _)    -> let yt y' = (y + h) - (y'-min)*h/(max-min)
                                                        in (x,yt p,x+w,yt p)
         C.save
         setLineStyle gl             
         moveTo x3 y3
         lineTo x4 y4
         C.stroke
         C.restore)
       let majlab = case sd of 
                      (Side _)  -> True
                      (Value _) -> False
       when (t == Major && majlab) $ do
            let s = if sc == Log then formatTick "10e%.1g" v else formatTick tf v
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

formatTick :: String -> Double -> String
formatTick tf p
    | tf /= ""      = Printf.printf tf p 
    | p == 0.0      = "0"
    | abs p > 1000  = Printf.printf "%1.1e" p
    | abs p < 0.001 = Printf.printf "%.3e" p
    | otherwise     = Printf.printf "%.2f" p
-- %g uses "whichever of %f, %e is smaller

-----------------------------------------------------------------------------
