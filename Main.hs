{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex
import Reflex.Dom
import Data.Text (Text, pack) 
import Data.Monoid ((<>)) 
import Data.Map (Map, fromList)
import Data.Time.Clock (UTCTime, utctDay, getCurrentTime)
import Data.Time.Calendar (Day, toGregorian, addDays)
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

-- Generate ticks until limit reached 
limitedTicks :: MonadWidget t m => Int -> m (Event t TickInfo)
limitedTicks limit = do
  now <- liftIO getCurrentTime 
  eTick <- tickLossy 0.01 now
  dCount <- count eTick
  dLimit <- holdUniqDyn $ fmap (limit<) dCount 
  switchPromptly eTick (never <$ updated dLimit)

addPoint :: Point -> Point -> Point
addPoint (p0x, p0y) (p1x, p1y) = (p0x + p1x, p0y + p1y)

type ChangeDate = Integer

sumOfTheDay :: MonadWidget t m => (Int, Int, Int) -> m (Event t ChangeDate)
sumOfTheDay (y,m,d) = do
  let segmentCount = lcm (lcm m d) y
      ymdAngle = toAngle (y,m,d)
      vectors = fmap (toPoint.ymdAngle) [0..segmentCount] 
      points = scanl addPoint (0.0,0.0) vectors

      xMax = maximum $ map fst points
      xMin = minimum $ map fst points
      yMax = maximum $ map snd points
      yMin = minimum $ map snd points

      width = xMax -xMin
      height = yMax -yMin
 
  -- Generate one event per segment to be displayed.
  eTick <- limitedTicks segmentCount

  -- A dynamically updating angle (with counter)
  dAngle <- foldDyn (\_ (c, angle) -> (c+1, ymdAngle c)) (0, 0) eTick

  -- A dynamically updating point
  let dPoint = fmap (toPoint.snd) dAngle
      ePoint = updated dPoint

  -- dynamically accumulating points
  dPath <- foldDyn addPoint (0.0,0.0) ePoint
  let ePath = updated dPath

  -- A dynamically updating indexed line segment
  -- The index is for listHoldWithKey
  dLine <- foldDyn (\p (index, (p0, p1))  -> (index+1, (p1,p)) ) (0, ((0,0), (0,0))) ePath

  
  -- A dynamically updating map from index to Maybe line. 
  -- The map will always have only one entry - the line to be
  -- added to the displayed segments.
  let dLineMap = fmap (\(i, l) -> fromList [(i, Just l)]) dLine

  -- Change to events and drop the first one (the initial value 
  -- introduced by foldDyn)
  eLineMap <- tailE $ updated dLineMap

  let sz = 450

  let boardAttrs = 
        fromList [ ("width" , pack $ show sz)
                 , ("height", pack $ show sz)
                 , ("viewBox", pack $ show xMin ++ " " ++ show yMin ++ " " ++ show width ++ " " ++ show height)
                 ]
  ev <- el "h2" $ do
            leftButton <- button "<<"
            text $ pack (show m ++ "/" ++ show d ++ "/" ++ show y)
            rightButton <- button ">>"

            let prevEvent =  (-1) <$ leftButton 
                nextEvent = 1 <$ rightButton 
            return (leftmost [prevEvent, nextEvent])

  elSvgns "svg" (constDyn boardAttrs) (listHoldWithKey mempty eLineMap $ showLine (width/200.0))
  return ev

sumOfDay :: MonadWidget t m => Day -> m (Event t ChangeDate)
sumOfDay uDay = do
  let (y0, m, d) = toGregorian uDay
      y = fromIntegral y0 `mod` 100
      dayWidget = sumOfTheDay (y,m,d)
  el "div" dayWidget

sumOfDayWidget :: MonadWidget t m => m ()
sumOfDayWidget = do
  now <- liftIO getCurrentTime 
  let today = utctDay now
  rec 
      dDay <- foldDyn (\(change) d -> addDays change d) today dayChange
      let eDay = updated dDay
          eSum = fmap sumOfDay eDay
          todaySum = sumOfDay today
      dynSumEvent <- widgetHold todaySum eSum
      let dayChange = switch $ current dynSumEvent
  return ()

header :: MonadWidget t m => m ()
header = text "Exponential sum of the day"

footer :: MonadWidget t m => m ()
footer = do
    text "For more explanation: "
    elAttr "a" ("href" =: "https://www.johndcook.com/blog/2017/11/02/recent-exponential-sums/") $ text "Recent exponential sums"

main = mainWidgetInElementById "main-wrapper" $ do 
    el "header" header
    el "article" sumOfDayWidget
    el "footer" footer

-- The svg attributes needed to display a line segment.
lineAttrs :: Float -> Segment -> Map Text Text
lineAttrs width ((x1,y1), (x2,y2)) =
  fromList [ ( "x1",    pack $ show x1)
           , ( "y1",    pack $ show y1)
           , ( "x2",    pack $ show x2)
           , ( "y2",    pack $ show y2)
           , ( "style", "stroke:red")
           , ( "stroke-width", pack $ show width)
           ]    

-- Use svg to display a line segment.
showLine :: MonadWidget t m => Float -> Int -> Segment -> m ()
showLine width _ segment = elSvgns "line" (constDyn $ lineAttrs width segment) $ return ()

-- Display an element in svg namespace
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
elSvgns t m ma = do
    (el, val) <- elDynAttrNS' (Just "http://www.w3.org/2000/svg") t m ma
    return val
