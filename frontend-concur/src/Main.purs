module Main where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Monad (bind)
import Data.Array.NonEmpty as NE
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect)
import Raytracker.API (addOrEditItem, retrieveItems, deleteItem)
import Raytracker.ISODate (ISODate, nowIsoDate)
import Raytracker.Item (Item)
import Raytracker.ItemForm (itemForm)
import Raytracker.ItemTable (ItemTableAction(..), itemTable)
import Raytracker.ItemType (ItemType(..))
import Raytracker.Timeline (drawTimeline)

initialItem :: forall m.MonadEffect m => m Item
initialItem = do
  now <- nowIsoDate
  pure {
    itemId: Nothing
  , itemType: Other
  , itemTitle: ""
  , itemNote: ""
  , itemStart: now
  , itemEnd: Nothing
  , itemAbandoned: false
  }

data MainAction = MainActionDelete Item | MainActionDoEdit Item | MainActionEnterEdit Item

mainSkeleton :: forall a. Widget HTML a
mainSkeleton = D.div [ P.className "container" ] [ mainWidget ]

timelineFromItems :: forall a.ISODate -> Array Item -> Widget HTML a
timelineFromItems today items = case NE.fromArray items of
  Nothing -> D.text "No items yet"
  Just items' -> D.h1' [D.text"Timeline"] <> drawTimeline today items'

mainWidgetAfterItems :: forall a. Item -> Either String (Array Item) -> Widget HTML a
mainWidgetAfterItems currentItem items' = case items' of
  Left e -> D.text ("error " <> e)
  Right items -> do
      today <- nowIsoDate
      let transformTableAction (ItemTableActionEdit i) = MainActionEnterEdit i
          transformTableAction (ItemTableActionDelete i) = MainActionDelete i
      totalAction <- (MainActionDoEdit <$> itemForm currentItem) <|> (transformTableAction <$> itemTable items) <|> timelineFromItems today items
      case totalAction of
        MainActionDoEdit i -> do
          newItems <- liftAff (addOrEditItem i)
          newCurrent <- initialItem
          mainWidgetAfterItems newCurrent newItems
        MainActionDelete i -> do
          case i.itemId of
            Nothing -> mainWidgetAfterItems currentItem items'
            Just itemId -> do
              newItems <- liftAff (deleteItem itemId)
              mainWidgetAfterItems currentItem newItems
        MainActionEnterEdit i -> do
          mainWidgetAfterItems i (Right items)


mainWidget :: forall a. Widget HTML a
mainWidget = do
  items' <- liftAff retrieveItems <|> D.text "Retrieving items..."
  item <- initialItem
  mainWidgetAfterItems item items'

main :: Effect Unit
main = runWidgetInDom "root" mainSkeleton
