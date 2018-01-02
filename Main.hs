{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Text (Text, pack) 
import Data.Map (Map, fromList)
import Data.Time.Clock (UTCTime, utctDay, getCurrentTime)
import Data.Time.Calendar (toGregorian)
import Control.Monad.Trans (liftIO)

type Point = (Float,Float)
type Segment = (Point,Point)

toAngle :: (Int, Int, Int) -> Int -> Float
toAngle (y,m,d) n = 
  let r = fromIntegral n
  in 2.0 * pi * (  r     / fromIntegral m
                +  r*r   / fromIntegral d
                +  r*r*r / fromIntegral y
                )

toPoint :: Float -> Point
toPoint angle = (cos angle,sin angle)

finiteTicks :: MonadWidget t m => UTCTime ->Int -> m (Event t TickInfo)
finiteTicks now limit = do
  eTick0 <- tickLossy 0.01 now

  dCount <- count eTick0
  dLimit <- holdUniqDyn $ fmap (limit<) dCount 

  let eCountPastLimit = ffilter id (updated dLimit) 

  switchPromptly eTick0 $ never <$ eCountPastLimit

addPoint :: Point -> Point -> Point
addPoint (p0x, p0y) (p1x, p1y) = (p0x + p1x, p0y + p1y)

sumOfTheDay :: MonadWidget t m => (Int, Int, Int) -> m ()
sumOfTheDay (y,m,d) = do
  now <- liftIO getCurrentTime 

  let segmentCount = lcm (lcm m d) $ y
      ymdAngle = toAngle (y,m,d)
      vectors = fmap (toPoint.ymdAngle) [0..segmentCount] 
      points = scanl addPoint (0.0,0.0) vectors

      xMax = maximum $ map fst points
      xMin = minimum $ map fst points
      yMax = maximum $ map snd points
      yMin = minimum $ map snd points

      width = xMax -xMin
      height = yMax -yMin
  
  eTick <- finiteTicks now segmentCount

  -- A dynamically updating angle (with counter)
  dAngle <- foldDyn (\_ (c, angle) -> (c+1, ymdAngle c)) (0, 0) eTick

  -- A dynamically updating point
  let dPoint = fmap (toPoint.snd) dAngle
      ePoint = updated dPoint

  -- dynamically accumulating points
  dPath <- foldDyn addPoint (0.0,0.0) ePoint
  let ePath = updated dPath

  -- A dynamically updating indexed line segment
  dLine <- foldDyn (\p (index, (p0, p1))  -> (index+1, (p1,p)) ) (0, ((0,0), (0,0))) ePath

  let dLineMap = fmap (\(i, l) -> fromList [(i, Just l)]) dLine

  eLineMap <- tailE $ updated dLineMap

  let boardAttrs = 
        fromList [ ("width" , pack $ show 400)
                 , ("height", pack $ show 400)
                 , ("viewBox", pack $ show (xMin) ++ " " ++ show (yMin) ++ " " ++ show width ++ " " ++ show height)
                 ]
  el "h2" $ do
      button "<<"
      text $ pack (show m ++ "/" ++ show d ++ "/" ++ show y)
      button ">>"
  elSvgns "svg" (constDyn boardAttrs) (listHoldWithKey mempty eLineMap $ showLine (width/200.0))
  return ()


main = mainWidget $ do 
  now <- liftIO getCurrentTime 
  let uDay = utctDay now
      (y0, m, d) = toGregorian uDay
      y = fromIntegral y0 `mod` 100
  elAttr "div" (fromList [("style", "text-align:center")]) $ do
    el "h1" $ text "Exponential sum of the day"
    sumOfTheDay (y,m,d)

-- The svg attributes needed to display a line segment.
lineAttrs :: Float -> Segment -> Map Text Text
lineAttrs width ((x1,y1), (x2,y2)) =
  fromList [ ( "x1",    pack $ show x1)
           , ( "y1",    pack $ show y1)
           , ( "x2",    pack $ show x2)
           , ( "y2",    pack $ show y2)
           , ( "style", "stroke:midnightblue")
           , ( "stroke-width", pack $ show $ width)
           ]    

-- Use svg to display a line segment.
showLine :: MonadWidget t m => Float -> Int -> Segment -> m ()
showLine width _ segment = elSvgns "line" (constDyn $ lineAttrs width segment) $ return ()

-- Display an element in svg namespace
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
elSvgns t m ma = do
    (el, val) <- elDynAttrNS' (Just "http://www.w3.org/2000/svg") t m ma
    return val
