module Raytracker.Item where

import Data.Maybe (Maybe)
import Raytracker.ISODate (ISODate)
import Raytracker.ItemType (ItemType)

type Item = {
    itemId :: Maybe Int
  , itemType :: ItemType
  , itemTitle :: String
  , itemNote :: String
  , itemStart :: ISODate
  , itemEnd :: Maybe ISODate
  , itemAbandoned :: Boolean
  }
