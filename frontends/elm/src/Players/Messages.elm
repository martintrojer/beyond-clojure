module Players.Messages exposing (..)

import Http
import Players.Models exposing (PlayerId, Player)


type Msg
    = FetchAllDone (List Player)
    | FetchAllFail Http.Error
    | ShowPlayers
    | ShowPlayer PlayerId
    | ChangeLevel PlayerId Int
    | SaveSuccess Player
    | SaveFail Http.Error
