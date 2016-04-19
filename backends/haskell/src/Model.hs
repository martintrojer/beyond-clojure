{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import Control.Monad
import Data.Aeson
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.TH
import Database.Esqueleto

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Player
  name String
  level Int
  deriving Show
|]

instance ToJSON Player where
  toJSON (Player name level) =
    object ["name" .= name, "level" .= level]

instance FromJSON Player where
  parseJSON (Object v) =
    Player <$> v .: "name"
           <*> v .: "level"
  parseJSON _ = mzero

getAllPlayers :: SqlPersistT (ResourceT (NoLoggingT IO)) [Player]
getAllPlayers = do
  players <- select $ from $ \player -> do
    orderBy [asc (player ^. PlayerName)]
    return player
  return $ map entityVal players

getPlayer :: String -> SqlPersistT (ResourceT (NoLoggingT IO)) (Maybe Player)
getPlayer pName = do
  players <- select $ from $ \player -> do
    where_ (player ^. PlayerName ==. val pName)
    return player
  return $ case players of
    (ePlayer:_) -> Just $ entityVal ePlayer
    _ -> Nothing

createPlayer :: String -> Int -> SqlPersistT (ResourceT (NoLoggingT IO)) ()
createPlayer pName pLevel = insert_ $ Player pName pLevel

deletePlayer :: String -> SqlPersistT (ResourceT (NoLoggingT IO)) ()
deletePlayer pName = delete $ from $ \name ->
  where_ (name ^. PlayerName ==. val pName)

setupDB :: SqlPersistT (NoLoggingT IO) ()
setupDB = do
  runMigration migrateAll
  insert_ $ Player "Sally" 2
  insert_ $ Player "Lance" 1
  insert_ $ Player "Aki" 3
  insert_ $ Player "Maria" 4
