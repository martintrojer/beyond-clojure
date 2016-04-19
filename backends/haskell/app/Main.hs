{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Database.Persist.Sqlite hiding (get)
import Snap

import App
import Model
import SqliteSnaplet

appInit :: SnapletInit App App
appInit = makeSnaplet "app" "a player db backend" Nothing $ do
  addRoutes [ ("", ifTop $ writeText "Welcome to the Players API v0.1")
            , ("players", method GET getPlayersHandler)
            , ("players/:player", method GET getPlayerHandler <|>
                                  method POST createPlayerHandler <|>
                                  method DELETE deletePlayerHandler)
            ]
  d <- nestSnaplet "db" db $ initSqlite setupDB
  return $ App d

main :: IO ()
main = serveSnaplet defaultConfig appInit
