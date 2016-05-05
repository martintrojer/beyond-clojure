module Players.Actions (..) where

import Players.Models exposing (PlayerId, Player)
import Http
import Hop


type Action
  = NoOp
  -- route actions
  | HopAction ()
  | EditPlayer PlayerId
  | ListPlayers

  -- rest actions
  | FetchAllDone (Result Http.Error (List Player))
  | CreatePlayer
  | CreatePlayerDone (Result Http.Error Player)
  | DeletePlayerIntent Player
  | DeletePlayer PlayerId
  | DeletePlayerDone PlayerId (Result Http.Error ())
  | ChangeName PlayerId String
  | ChangeLevel PlayerId Int
  | SaveDone (Result Http.Error Player)

  -- misc
  | TaskDone ()
