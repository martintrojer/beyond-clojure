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
            , ("players", method GET getPlayersHandler <* addCors <|>
                          method POST createPlayerHandler <* addCors <|>
                          method OPTIONS addCors)
            , ("players/:id", method GET getPlayerHandler <* addCors <|>
                              method PUT updatePlayerHandler <* addCors <|>
                              method DELETE deletePlayerHandler <* addCors <|>
                              method OPTIONS addCors)
            ]
  d <- nestSnaplet "db" db $ initSqlite setupDB
  return $ App d

main :: IO ()
main = serveSnaplet defaultConfig appInit
