module Raytracker.ISODate where

import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Bifunctor (lmap)
import Data.Bounded (bottom)
import Data.Date (Date)
import Data.DateTime (DateTime(..), date)
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformat)
import Data.Function ((<<<))
import Data.Functor (map, (<$>))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Data.String.Read (class Read)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDate)

newtype ISODate = ISODate Date

isoFormatter :: Formatter
isoFormatter = YearFull : Placeholder "-" : MonthTwoDigits : Placeholder "-" : DayOfMonthTwoDigits : Nil

instance DecodeJson ISODate where
  decodeJson json = case toString json of
    Nothing -> Left (TypeMismatch "expected string for ISO date, got something else")
    Just s -> lmap TypeMismatch ((ISODate <<< date) <$> unformat isoFormatter s)


instance EncodeJson ISODate where
  encodeJson x = fromString (show x)

instance Show ISODate where
  show (ISODate s) = format isoFormatter (DateTime s bottom)

instance Read ISODate where
  read = hush <<< (map (ISODate <<< date)) <<< unformat isoFormatter

nowIsoDate :: forall m. MonadEffect m => m ISODate
nowIsoDate = ISODate <$> liftEffect nowDate
