module Raytracker.ISODateTime where

import Data.Argonaut.Core (fromString, toString)
import Data.Show(class Show)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformat)
import Data.Functor ((<$>))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

newtype ISODateTime = ISODateTime DateTime

isoFormatter :: Formatter
isoFormatter = YearFull : Placeholder "-" : MonthTwoDigits : Placeholder "-" : DayOfMonthTwoDigits : Placeholder "T" : Hours24 : Placeholder ":" : MinutesTwoDigits : Placeholder ":" : SecondsTwoDigits : Placeholder "." : Milliseconds : Placeholder "Z" : Nil

instance DecodeJson ISODateTime where
  decodeJson json = case toString json of
    Nothing -> Left (TypeMismatch "expected string for ISO datetime, got something else")
    Just s -> lmap TypeMismatch (ISODateTime <$> unformat isoFormatter s)


instance EncodeJson ISODateTime where
  encodeJson (ISODateTime x) = fromString (format isoFormatter x)

instance Show ISODateTime where
  show (ISODateTime s) = format isoFormatter s
