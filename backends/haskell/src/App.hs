{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Snap
import Snap.Extras.JSON
import GHC.Generics

import SqliteSnaplet
import Model

data App = App { _db :: Snaplet PersistState}

makeLenses ''App

instance HasPersistPool (Handler a App) where
  getPersistPool = with db getPersistPool

notFound :: Handler App App ()
notFound = modifyResponse $ setResponseStatus 404 "Not found"

error400 :: String -> Handler App App ()
error400 err = modifyResponse $ setResponseStatus 400 (BS.pack err)

getPlayer' :: Handler App App (Maybe PlayerWKey)
getPlayer' =
  runMaybeT $ do
    param <- MaybeT $ getParam "id"
    MaybeT . runPersist . getPlayer . read . BS.unpack $ param

addCors :: Handler App App ()
addCors = do
  modifyResponse $ addHeader "Access-Control-Allow-Origin" "*"
  modifyResponse $ addHeader "Access-Control-Allow-Methods" "GET, DELETE, POST, PUT, OPTIONS"
  modifyResponse $ addHeader "Access-Control-Allow-Credentials" "true"
  modifyResponse $ addHeader "Access-Control-Allow-Headers" "Content-Type, authorization, Origin, Host, User-Agent, Access-Control-Request-Headers, Referer, Connection, Accept, Accept-Language, Access-Control-Request-Method, X-Forwarded-For, Accept-Encoding, X-Real-Ip"

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

data CreateParams =
  CreateParams { name :: String
               , level :: Int }
  deriving(Show, Generic)

instance FromJSON CreateParams

createPlayerHandler :: Handler App App ()
createPlayerHandler = do
  player <- getPlayer'
  body :: Either String CreateParams <- getJSON
  case (player, body) of
    (Nothing, Right (CreateParams n l)) -> do
      newPlayer <- runPersist $ createPlayer n l
      modifyResponse $ setResponseStatus 201 "Created"
      writeJSON newPlayer
    (Nothing, Left err) ->
      error400 err
    (Just _, _) -> modifyResponse $ setResponseStatus 400 "Player exists"

updatePlayerHandler :: Handler App App ()
updatePlayerHandler = do
  player <- getPlayer'
  body :: Either String CreateParams <- getJSON
  case (player, body) of
    (Just (PlayerWKey i _ _), Right (CreateParams n l)) -> do
      newPlayer <- runPersist $ updatePlayer i n l
      modifyResponse $ setResponseStatus 200 "Updated"
      writeJSON newPlayer
    (Just _, Left err) ->
      error400 err
    _ -> notFound

deletePlayerHandler :: Handler App App ()
deletePlayerHandler = do
  player <- getPlayer'
  case player of
    Just p -> do
      _ <- runPersist . deletePlayer $ Model.id p
      modifyResponse $ setResponseStatus 200 "Deleted"
      writeJSON p
    Nothing -> notFound
