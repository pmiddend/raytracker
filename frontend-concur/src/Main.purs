module Main where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Monad (bind)
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect)
import Raytracker.API (addOrEdit, retrieveItems)
import Raytracker.ISODate (nowIsoDate)
import Raytracker.Item (Item)
import Raytracker.ItemForm (itemForm)
import Raytracker.ItemTable (itemTable)
import Raytracker.ItemType (ItemType(..))

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

data MainAction = MainActionDoEdit Item | MainActionEnterEdit Item

mainSkeleton :: forall a. Widget HTML a
mainSkeleton = D.div [ P.className "container" ] [ mainWidget ]

mainWidgetAfterItems :: forall a. Item -> Either String (Array Item) -> Widget HTML a
mainWidgetAfterItems currentItem items' = case items' of
  Left e -> D.text ("error " <> e)
  Right items -> do
      totalAction <- (MainActionDoEdit <$> itemForm currentItem) <|> (MainActionEnterEdit <$> itemTable items)
      case totalAction of
        MainActionDoEdit i -> do
          newItems <- liftAff (addOrEdit i)
          newCurrent <- initialItem
          mainWidgetAfterItems newCurrent newItems
        MainActionEnterEdit i -> do
          mainWidgetAfterItems i (Right items)


mainWidget :: forall a. Widget HTML a
mainWidget = do
  items' <- liftAff retrieveItems <|> D.text "Retrieving items..."
  item <- initialItem
  mainWidgetAfterItems item items'

main :: Effect Unit
main = runWidgetInDom "root" mainSkeleton
