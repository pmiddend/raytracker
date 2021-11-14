module Raytracker.API where

import Affjax as AX
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Control.Applicative (pure)
import Control.Bind ((>>=))
import Control.Monad (bind)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Function ((>>>))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Raytracker.Item (Item)


retrieveItems :: Aff (Either String (Array Item))
retrieveItems = do
  response <- (lmap AX.printError) <$> AX.get json "/timeline"
  pure (response >>= (_.body >>> decodeJson >>> lmap printJsonDecodeError))
  
addOrEdit :: Item -> Aff (Either String (Array Item))
addOrEdit i = do
  response <- (lmap AX.printError) <$> AX.post json "/timeline" (Just (Json (encodeJson i)))
  pure (response >>= (_.body >>> decodeJson >>> lmap printJsonDecodeError))
