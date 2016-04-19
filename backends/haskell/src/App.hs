{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module App where

import Control.Lens
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as BS
import Snap
import Snap.Extras.JSON

import SqliteSnaplet
import Model

data App = App { _db :: Snaplet PersistState}

makeLenses ''App

instance HasPersistPool (Handler a App) where
  getPersistPool = with db getPersistPool

notFound :: Handler App App ()
notFound = modifyResponse $ setResponseStatus 404 "Not found"

getPlayer' :: Handler App App (Maybe Player)
getPlayer' = do
  player <- runMaybeT $ do
    param <- MaybeT $ getParam "player"
    MaybeT . runPersist . getPlayer $ BS.unpack param
  return player

getPlayersHandler :: Handler App App ()
getPlayersHandler = do
  users <- runPersist getAllPlayers
  writeJSON users

getPlayerHandler :: Handler App App ()
getPlayerHandler = do
  player <- getPlayer'
  case player of
    Just p -> writeJSON p
    Nothing -> notFound

createPlayerHandler :: Handler App App ()
createPlayerHandler = do
  player <- getPlayer'
  name <- getParam "player"
  level <- getPostParam "level"
  case (player, name, level) of
    (Nothing, Just n, Just l) -> do
      runPersist $ createPlayer (BS.unpack n) $ read (BS.unpack l)
      modifyResponse $ setResponseStatus 201 "Created"
    (Just _, _, _) -> modifyResponse $ setResponseStatus 400 "Player exists"
    _ -> notFound

deletePlayerHandler :: Handler App App ()
deletePlayerHandler = do
  player <- getPlayer'
  case player of
    Just p -> do
      _ <- runPersist . deletePlayer $ playerName p
      modifyResponse $ setResponseStatus 204 ""
    Nothing -> notFound
