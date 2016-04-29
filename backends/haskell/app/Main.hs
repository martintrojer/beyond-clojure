{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Applicative
import Snap

import App
import Model
import SqliteSnaplet

appInit :: SnapletInit App App
appInit = makeSnaplet "app" "a player db backend" Nothing $ do
  addRoutes [ ("", ifTop $ writeText "Welcome to the Players API v0.1")
            , ("players", method GET getPlayersHandler <* addCors)
            , ("players/:player", method GET getPlayerHandler <* addCors <|>
                                  method POST createPlayerHandler <* addCors <|>
                                  method DELETE deletePlayerHandler <* addCors)
            ]
  d <- nestSnaplet "db" db $ initSqlite setupDB
  return $ App d

main :: IO ()
main = serveSnaplet defaultConfig appInit
