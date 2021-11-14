module Raytracker.ItemTable where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Maybe (maybe)
import Raytracker.Item (Item)

data ItemTableAction = Item

itemTable :: Array Item -> Widget HTML Item
itemTable items =
  let
    headers = [ "Actions", "ID", "Type", "Title", "Note", "Start", "End", "Abandoned" ]
    makeRow item = D.tr'
      [ D.td' [ D.a [ const item <$> P.onClick, P.href "/#" ] [ D.text "Edit" ] ]
      , D.td' [ D.text (maybe "" show item.itemId) ]
      , D.td' [ D.text (show item.itemType) ]
      , D.td' [ D.text item.itemTitle ]
      , D.td' [ D.text item.itemNote ]
      , D.td' [ D.text (show item.itemStart) ]
      , D.td' [ D.text (maybe "" show item.itemEnd) ]
      , D.td' [ D.text (show item.itemAbandoned) ]
      ]
  in
    D.table
      [ P.className "table" ]
      [ D.thead' [ D.tr' ((\x -> D.th' [ D.text x ]) <$> headers) ]
      , D.tbody' (makeRow <$> items)
      ]
