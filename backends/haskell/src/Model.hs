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

import Data.Aeson
import Data.Int
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist.TH
import Database.Esqueleto
import qualified GHC.Generics as GG

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Player
  pname String
  plevel Int
  deriving Show
|]

data PlayerWKey = PlayerWKey { id :: Int64
                             , name :: String
                             , level :: Int
                             } deriving (Show, GG.Generic)

instance ToJSON PlayerWKey

entityToPlayerKey :: Entity Player -> PlayerWKey
entityToPlayerKey e =
  PlayerWKey (fromSqlKey k) n l
  where k = entityKey e
        Player n l = entityVal e

getAllPlayers :: SqlPersistT (ResourceT (NoLoggingT IO)) [PlayerWKey]
getAllPlayers = do
  players <- select $ from $ \player -> do
    orderBy [asc (player ^. PlayerPname)]
    return player
  return $ map entityToPlayerKey players

getPlayer :: Int64 -> SqlPersistT (ResourceT (NoLoggingT IO)) (Maybe PlayerWKey)
getPlayer pk = do
  players <- select $ from $ \player -> do
    where_ (player ^. PlayerId ==. valkey pk)
    return player
  return $ case players of
    (ePlayer:_) -> Just $ entityToPlayerKey ePlayer
    _ -> Nothing

createPlayer :: String -> Int -> SqlPersistT (ResourceT (NoLoggingT IO)) PlayerWKey
createPlayer n l = do
  k <- insert $ Player n l
  return $ PlayerWKey (fromSqlKey k) n l

updatePlayer :: Int64 -> String -> Int -> SqlPersistT (ResourceT (NoLoggingT IO)) PlayerWKey
updatePlayer pk n l = do
  update $ \p -> do
    set p [ PlayerPname =. val n, PlayerPlevel =. val l ]
    where_ (p ^. PlayerId ==. valkey pk)
  return $ PlayerWKey pk n l

deletePlayer :: Int64 -> SqlPersistT (ResourceT (NoLoggingT IO)) ()
deletePlayer pk =
  delete $ from $ \p ->
  where_ (p ^. PlayerId ==. valkey pk)

setupDB :: SqlPersistT (NoLoggingT IO) ()
setupDB = do
  runMigration migrateAll
  insert_ $ Player "Sally" 2
  insert_ $ Player "Lance" 1
  insert_ $ Player "Aki" 3
  insert_ $ Player "Maria" 4
