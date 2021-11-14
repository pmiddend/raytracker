module Raytracker.ItemForm where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (unsafeTargetValue)
import Concur.React.Props as P
import Control.Applicative (pure)
import Data.BooleanAlgebra (not)
import Data.Eq ((==))
import Data.Function (const, (<<<))
import Data.Functor ((<$), (<$>))
import Data.Maybe (fromMaybe, maybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Read (read)
import Prelude (bind)
import Raytracker.Item (Item)
import Raytracker.ItemType (allItemTypes)

data ItemFormAction
  = ItemFormActionChange (Item -> Item)
  | ItemFormActionSubmit

itemFormText :: forall a. String -> String -> String -> (String -> a) -> Widget HTML a
itemFormText id value title handler = D.div [ P.className "mb-3 form-floating" ]
  [ D.input
      [ P._type "text"
      , P.value value
      , P._id ("form-" <> id)
      , P.className "form-control"
      , handler <<< unsafeTargetValue <$> P.onChange
      ]
  , D.label [ P.htmlFor ("form-" <> id) ] [ D.text title ]
  ]

itemForm :: Item -> Widget HTML Item
itemForm i = do
  a <- itemFormInner i
  case a of
    ItemFormActionChange f -> itemForm (f i)
    ItemFormActionSubmit -> pure i

itemFormInner :: Item -> Widget HTML ItemFormAction
itemFormInner i = D.h1' [ D.text (maybe "Add" (const ("Edit " <> i.itemTitle)) i.itemId) ] <> D.form'
  [ itemFormText "title" i.itemTitle "Title" (\x -> ItemFormActionChange (\oi -> oi { itemTitle = x }))
  , D.div [ P.className "mb-3 form-floating" ]
      [ D.textarea
          [ P._id "form-note"
          , P.className "form-control"
          , (\x -> ItemFormActionChange (\oi -> oi { itemNote = x })) <<< unsafeTargetValue <$> P.onChange
          ]
          [ D.text i.itemNote ]
      , D.label [ P.htmlFor "form-note" ] [ D.text "Notes" ]
      ]
  , D.div [ P.className "mb-3 form-floating" ]
      [ D.select
          [ P.className "form-select"
          , P._id "form-type"
          , (\x -> ItemFormActionChange (\oi -> oi { itemType = fromMaybe oi.itemType (read x) })) <<< unsafeTargetValue <$> P.onChange
          ]
          ((\it -> D.option [ P.value (show it), P.selected (it == i.itemType) ] [ D.text (show it) ]) <$> allItemTypes)
      , D.label [ P.htmlFor "form-type" ] [ D.text "Type" ]
      ]
  , D.div [ P.className "mb-3 form-floating" ]
      [ D.input
          [ P._id "form-start"
          , P._type "date"
          , P.className "form-control"
          , P.value (show i.itemStart)
          , (\x -> ItemFormActionChange (\oi -> oi { itemStart = fromMaybe oi.itemStart (read x) })) <<< unsafeTargetValue <$> P.onChange
          ]
      , D.label [ P.htmlFor "form-start" ] [ D.text "Start" ]
      ]
  , D.div [ P.className "mb-3 form-floating" ]
      [ D.input
          [ P._id "form-end"
          , P._type "date"
          , P.className "form-control"
          , P.value (maybe "" show i.itemEnd)
          , (\x -> ItemFormActionChange (\oi -> oi { itemEnd = read x })) <<< unsafeTargetValue <$> P.onChange
          ]
      , D.label [ P.htmlFor "form-end" ] [ D.text "End" ]
      ]
  , D.div [ P.className "mb-3 form-check form-switch" ]
      [ D.input
          [ P._id "form-abandoned"
          , P._type "checkbox"
          , P.className "form-check-input"
          , P.checked i.itemAbandoned
          , (\_ -> ItemFormActionChange (\oi -> oi { itemAbandoned = not oi.itemAbandoned })) <<< unsafeTargetValue <$> P.onChange
          ]
      , D.label [ P.htmlFor "form-end" ] [ D.text "Abandoned" ]
      ]
  , D.button [ ItemFormActionSubmit <$ P.onClick, P.className "btn btn-primary" ] [ D.text "Submit" ]
  ]

