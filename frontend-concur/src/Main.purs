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
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Data.String.Read (read)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect)
import Raytracker.API (addOrEdit, retrieveItems)
import Raytracker.ISODate (ISODate(..), nowIsoDate)
import Raytracker.Item (Item)
import Raytracker.ItemForm (itemForm)
import Raytracker.ItemTable (itemTable)
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
mainWidget =
  let philippBday :: Maybe ISODate
      philippBday = read "2021-08-21"
      hannaBday :: Maybe ISODate
      hannaBday = read "2021-07-09"
      hpStart :: Maybe ISODate
      hpStart = read "2017-05-02"
      simpleItem :: ISODate -> Maybe ISODate -> String -> Item
      simpleItem itemStart itemEnd itemTitle = { itemId: Nothing, itemType: Book, itemTitle: itemTitle, itemNote: "", itemStart, itemEnd, itemAbandoned: false }
  in case philippBday, hannaBday, hpStart of
--    Just philippBday', Just hannaBday', Just hpStart' -> drawTimeline (simpleItem hannaBday' Nothing "Hanna" NE.: NE.singleton (simpleItem hpStart' (Just philippBday') "Philipp"))
    Just philippBday', Just hannaBday', Just hpStart' -> drawTimeline (NE.singleton (simpleItem hpStart' (Just philippBday') "Philipp"))
    _, _, _ -> D.text "invalid!"
  -- items' <- liftAff retrieveItems <|> D.text "Retrieving items..."
  -- item <- initialItem
  -- mainWidgetAfterItems item items'

main :: Effect Unit
main = runWidgetInDom "root" mainSkeleton
