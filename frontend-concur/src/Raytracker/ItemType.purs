module Raytracker.ItemType where

import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array (find)
import Data.Bounded (class Bounded, bottom, top)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), enumFromTo)
import Data.Eq ((==), class Eq)
import Data.Function ((<<<))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.Read (class Read)
import Data.Unfoldable1 (class Unfoldable1)

data ItemType = Book | Show | Person | Game | Other

derive instance Eq ItemType
derive instance Ord ItemType

instance Enum ItemType where
  succ Book = Just Show
  succ Show = Just Person
  succ Person = Just Game
  succ Game = Just Other
  succ Other = Nothing
  pred Book = Nothing
  pred Show = Just Book
  pred Person = Just Show
  pred Game = Just Person
  pred Other = Just Game

instance Bounded ItemType where
  top = Other
  bottom = Book

instance BoundedEnum ItemType where
  cardinality = Cardinality 5
  toEnum 0 = Just Book
  toEnum 1 = Just Show
  toEnum 2 = Just Person
  toEnum 3 = Just Game
  toEnum 4 = Just Other
  toEnum _ = Nothing
  fromEnum Book = 0
  fromEnum Show = 1
  fromEnum Person = 2
  fromEnum Game = 3
  fromEnum Other = 4

instance Show ItemType where
  show Book = "Book"
  show Show = "Show"
  show Person = "Person"
  show Game = "Game"
  show Other = "Other"

instance Read ItemType where
  read s = find (\it -> show it == s) allItemTypes

allElements :: forall a. BoundedEnum a => Array a
allElements = enumFromTo bottom top

allItemTypes :: forall a.Unfoldable1 a => a ItemType
allItemTypes = enumFromTo bottom top

instance DecodeJson ItemType where
  decodeJson j = case toString j of
    Nothing -> Left (TypeMismatch "expected string for itemtype, got something else")
    Just s -> fromMaybe (Left (TypeMismatch ("invalid string for item type: " <> s))) (Right <$> (find (\x -> show x == s) allElements))

instance EncodeJson ItemType where
  encodeJson = fromString <<< show
