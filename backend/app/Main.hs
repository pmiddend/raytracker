{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class(liftIO)
import Control.Monad(when, void)
import Control.Exception(bracket)
import Data.Text(pack)

import Data.Time (UTCTime)
import Servant
import Data.Aeson(FromJSON, ToJSON)
import GHC.Generics(Generic)
import Network.Wai(Application)
import Network.Wai.Handler.Warp(run)

import Database.SQLite.Simple
import Database.SQLite.Simple.Ok(Ok(..))
import Database.SQLite.Simple.FromRow(FromRow(fromRow))
import Database.SQLite.Simple.FromField(FromField(fromField), ResultError(Incompatible), fieldData, returnError)
import Database.SQLite.Simple.ToField(ToField(toField))

import Options.Applicative(Parser, switch, option, auto, long, help, strOption, info, helper, fullDesc, progDesc, header, execParser, (<**>))

type SqliteURL = String

newtype DBItem = DBItem (Item Int)

unwrapDbItem :: DBItem -> Item Int
unwrapDbItem (DBItem i) = i

instance FromRow DBItem where
  fromRow = DBItem <$> (Item <$> field <*> field <*> field <*> field <*> field <*> field <*> field)

listItems :: SqliteURL -> IO [DBItem]
listItems sqliteUrl = bracket (open sqliteUrl) close (\conn -> query_ conn "SELECT id, itemType, title, note, start, end, abandoned from Items" :: IO [DBItem])

insertItem :: forall a.SqliteURL -> Item a -> IO ()
insertItem sqliteUrl (Item _itemId itemType itemTitle itemNote itemStart _itemEnd _itemAbandoned) =
  bracket
    (open sqliteUrl)
    close
    (\conn -> void (execute conn "INSERT INTO Items (itemType, title, note, start, abandoned) VALUES (?, ?, ?, ?, 0)" (itemType, itemTitle, itemNote, itemStart)))

updateItem :: forall a.SqliteURL -> Item Int -> IO ()
updateItem sqliteUrl (Item itemId itemType itemTitle itemNote itemStart itemEnd itemAbandoned) =
  bracket
    (open sqliteUrl)
    close
    (\conn -> void (execute conn "UPDATE Items SET itemType=?, title=?, note=?, start=?, end=?, abandoned=? WHERE id = ?" (itemType, itemTitle, itemNote, itemStart, itemEnd, itemAbandoned, itemId)))

data ItemType = Book | Show | Person | Game | Other deriving (Eq, Show, Generic)

instance FromField ItemType where
  fromField f = case fieldData f of
    SQLText "Book" -> Ok Book
    SQLText "Show" -> Ok Show
    SQLText "Person" -> Ok Person
    SQLText "Game" -> Ok Game
    SQLText "Other" -> Ok Other
    _ -> returnError Incompatible f "invalid"

instance ToJSON ItemType
instance FromJSON ItemType

instance ToField ItemType where
  toField = SQLText . pack . show

data Item a = Item {
    itemId :: a
  , itemType :: ItemType
  , itemTitle :: String
  , itemNote :: String
  , itemStart :: UTCTime
  , itemEnd :: Maybe UTCTime
  , itemAbandoned :: Bool
  } deriving (Eq, Show, Generic, Functor)

instance ToJSON a => ToJSON (Item a)
instance FromJSON a => FromJSON (Item a)

type TimelineAPI = "timeline" :> Get '[JSON] [Item Int]
              :<|> "timeline" :> ReqBody '[JSON] (Item (Maybe Int)) :> Post '[JSON] [Item Int]

server :: FilePath -> Server TimelineAPI
server dbPath = listRequest :<|> putPostRequest
  where listRequest = do
          items <- liftIO (listItems dbPath)
          pure (unwrapDbItem <$> items)
        putPostRequest item = do
          liftIO $ case itemId item of
            Nothing -> insertItem dbPath item
            Just i -> updateItem dbPath (const i <$> item)
          listRequest

timelineAPI :: Proxy TimelineAPI
timelineAPI = Proxy

app :: FilePath -> Application
app dbPath = serve timelineAPI (server dbPath)

data CLI = CLI {
    port :: Int
  , dbPath :: FilePath
  , dbCreate :: Bool
  }

cli :: Parser CLI
cli = CLI <$> (option auto (long "port" <> help "port to bind to")) <*> (strOption (long "db-path" <> help "path to the SQLite db to store results in")) <*> (switch (long "db-create" <> help "create the database from scratch"))

runCli :: IO CLI
runCli = execParser opts
  where opts = info (cli <**> helper)
                 (fullDesc <> progDesc "server program for raytracker" <> header "raytracker-backend - the backend server for the raytracker web application")

main :: IO ()
main = do
  cli <- runCli
  when (dbCreate cli) $ do
    conn <- open (dbPath cli)
    execute_ conn "CREATE TABLE Items (id INTEGER PRIMARY KEY AUTOINCREMENT, itemType TEXT NOT NULL, title TEXT NOT NULL, note TEXT NOT NULL, start TEXT NOT NULL, end TEXT, abandoned INTEGER NOT NULL, UNIQUE (title, itemType))"
    putStrLn ("Created new DB in " <> dbPath cli)
  run (port cli) (app (dbPath cli))
