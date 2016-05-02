{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Monad
import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Aeson
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

error400 :: String -> Handler App App ()
error400 err = modifyResponse $ setResponseStatus 400 (BS.pack err)

getPlayer' :: Handler App App (Maybe Player)
getPlayer' =
  runMaybeT $ do
    param <- MaybeT $ getParam "player"
    MaybeT . runPersist . getPlayer $ BS.unpack param

addCors :: Handler App App ()
addCors = do
  modifyResponse $ addHeader "Access-Control-Allow-Origin" "*"
  modifyResponse $ addHeader "Access-Control-Allow-Methods" "GET, DELETE, POST, OPTIONS"

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
  CreateParams { level :: Int }
  deriving(Show)

instance FromJSON CreateParams where
  parseJSON (Object v) =
    CreateParams <$> v .: "level"
  parseJSON _ = mzero

createPlayerHandler :: Handler App App ()
createPlayerHandler = do
  player <- getPlayer'
  name <- getParam "player"
  body :: Either String CreateParams <- getJSON
  case (player, name, body) of
    (Nothing, Just n, Right params) -> do
      runPersist $ createPlayer (BS.unpack n) $ level params
      modifyResponse $ setResponseStatus 201 "Created"
    (Nothing, _, Left err) ->
      error400 err
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
