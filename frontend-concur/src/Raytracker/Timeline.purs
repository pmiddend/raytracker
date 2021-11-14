module Raytracker.Timeline where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Array.NonEmpty as NE
import Data.Bounded (bottom, top)
import Data.Date (Month(..), Year(..), canonicalDate)
import Data.Enum (enumFromTo, fromEnum, succ, toEnum)
import Data.Field ((/))
import Data.Functor ((<$>))
import Data.Int (floor, quot, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ring ((*), (+), (-))
import Data.Semigroup ((<>))
import Data.Semigroup.Foldable (maximum, minimum)
import Data.Show (show)
import Data.Time.Duration (Milliseconds(..), fromDuration)
import Raytracker.ISODate (ISODate(..), diff, year)
import Raytracker.Item (Item)

maybeIncreaseYear :: Year -> Int -> Year
maybeIncreaseYear inputYear inputN = fromMaybe inputYear (maybeIncreaseYear' (Just inputYear) inputN)
  where maybeIncreaseYear' :: Maybe Year -> Int -> Maybe Year
        maybeIncreaseYear' (Just y) 0 = Just y
        maybeIncreaseYear' (Just y) n = maybeIncreaseYear' (succ y) (n-1)
        maybeIncreaseYear' Nothing _ = Nothing


drawTimeline :: forall a. NE.NonEmptyArray Item -> Widget HTML a
drawTimeline items =
  let latestStartOrEnd :: Item -> ISODate
      latestStartOrEnd i = fromMaybe (i.itemStart) i.itemEnd
      minDateItems :: ISODate
      minDateItems = minimum (_.itemStart <$> items)
      maxDateItems :: ISODate
      maxDateItems = maximum (latestStartOrEnd <$> items)
      startFirstYear :: ISODate
      startFirstYear = ISODate (canonicalDate (year minDateItems) January bottom)
      endLastYear :: ISODate
      endLastYear = ISODate (canonicalDate (maybeIncreaseYear (year maxDateItems) 2) December top)
      timesheetWidth :: Int
      timesheetWidth = 720
      sectionWidth :: Int
      sectionWidth = quot timesheetWidth ((fromEnum (year endLastYear) - fromEnum (year startFirstYear)) + 1)
      makeSection :: Year -> Widget HTML a
      makeSection year = D.section [ P.style {
                                        width: show sectionWidth <> "px"
                                             } ] [ D.text (show (fromEnum year)) ]
      sections :: Widget HTML a
      sections = D.div [P.className "scale"] (makeSection <$> enumFromTo (year minDateItems) (year endLastYear))
      formatDate :: Item -> String
      formatDate i = case i.itemEnd of
        Just end -> show i.itemStart <> "-" <> show end
        Nothing -> show i.itemStart
      distanceTotalMs :: Milliseconds
      distanceTotalMs = diff endLastYear startFirstYear
      determineMarginPx :: ISODate -> Int
      determineMarginPx itemStart =
        let distanceFromStart :: Milliseconds
            distanceFromStart = diff itemStart startFirstYear
        in floor (unwrap distanceFromStart / unwrap distanceTotalMs * (toNumber timesheetWidth))
      determineDurationPx :: ISODate -> Maybe ISODate -> Int
      determineDurationPx start end' = case end' of
        Nothing -> determineDurationPx start (Just endLastYear)
        Just end ->
          let diffMs :: Milliseconds
              diffMs = diff end start
          in floor (unwrap diffMs / unwrap distanceTotalMs * (toNumber timesheetWidth))
      makeItem :: Item -> Widget HTML a
      makeItem i = D.li' [
          D.span [
             P.className "bubble bubble-lorem"
           , P.style {
                 "margin-left": show (determineMarginPx i.itemStart) <> "px"
               , "width": show (determineDurationPx i.itemStart i.itemEnd) <> "px"
             }
           , P._data {
              "duration": "12"
              } 
           ] [  ]
        , D.span [ P.className "date" ] [ D.text (formatDate i) ]
        , D.span [ P.className "label" ] [ D.text i.itemTitle ]
        ]
      dataList :: Widget HTML a
      dataList = D.ul [ P.className "data" ] (NE.toArray (makeItem <$> items))
  in D.div [ P.className "timesheet color-scheme-alternative" ] [
    sections
  , dataList
  ]
