{-# LANGUAGE OverloadedStrings #-}

module Spec where

import Data.Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Test.Hspec
import Snap
import Snap.Test
import qualified Snap.Snaplet.Test as ST

import App
import Model
import SqliteSnaplet

appInit :: SnapletInit App App
appInit = makeSnaplet "app" "a player db backend" Nothing $ do
  addRoutes []
  d <- nestSnaplet "db" db $ initSqlite setupDB
  return $ App d

main :: IO ()
main = hspec $ do
  describe "basic endpoints" $ do
    it "get all players" $ do
      res <- ST.runHandler Nothing (get (BS.pack "") M.empty) getPlayersHandler appInit
      case res of
        Left err -> expectationFailure $ unpack err
        Right resp -> do
          assertSuccess resp
          body <- getResponseBody resp
          body `shouldBe` "[{\"name\":\"Aki\",\"level\":3},{\"name\":\"Lance\",\"level\":1},{\"name\":\"Maria\",\"level\":4},{\"name\":\"Sally\",\"level\":2}]"
