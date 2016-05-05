module Players.Effects (..) where
import Effects exposing (Effects)
import Http
import Json.Decode as Decode exposing ((:=))
import Json.Encode exposing (encode, object, string, int)
import Task
import Players.Models exposing (Player, PlayerId)
import Players.Actions exposing (..)


fetchAll : Effects Action
fetchAll =
  Http.get collectionDecoder fetchAllUrl
    |> Task.toResult
    |> Task.map FetchAllDone
    |> Effects.task


fetchAllUrl : String
fetchAllUrl =
  "http://localhost:8000/players"


playerParams : Player -> String
playerParams player =
  let
    params =
      object
        [ ( "name", string player.name )
        , ( "level", int player.level )
        ]
  in
    encode 0 params


createPlayer: Player -> Effects Action
createPlayer player =
  let
    config =
      { verb = "POST"
      , headers = [ ( "Content-Type", "application/json" ) ]
      , url = createUrl player.id
      , body = Http.string (playerParams player)
      }
  in
    Http.send Http.defaultSettings config
      |> Http.fromJson memberDecoder
      |> Task.toResult
      |> Task.map CreatePlayerDone
      |> Effects.task


createUrl : PlayerId -> String
createUrl id =
  "http://localhost:8000/players/" ++ (toString id)


updatePlayer : Player -> Effects Action
updatePlayer player =
  let
    config =
      { verb = "PUT"
      , headers = [ ( "Content-Type", "application/json" ) ]
      , url = deleteUrl player.id
      , body = Http.string (playerParams player)
      }
  in
    Http.send Http.defaultSettings config
      |> Http.fromJson memberDecoder
      |> Task.toResult
      |> Task.map SaveDone
      |> Effects.task


deletePlayer : PlayerId -> Effects Action
deletePlayer playerId =
  let
    config =
      { verb = "DELETE"
      , headers = [ ( "Content-Type", "application/json" ) ]
      , url = deleteUrl playerId
      , body = Http.empty
      }
  in
    Http.send Http.defaultSettings config
      |> Http.fromJson (Decode.succeed ())
      |> Task.toResult
      |> Task.map (DeletePlayerDone playerId)
      |> Effects.task


deleteUrl : PlayerId -> String
deleteUrl playerId =
  "http://localhost:8000/players/" ++ (toString playerId)



-- DECODERS


collectionDecoder : Decode.Decoder (List Player)
collectionDecoder =
  Decode.list memberDecoder


memberDecoder : Decode.Decoder Player
memberDecoder =
  Decode.object3
    Player
    ("id" := Decode.int)
    ("name" := Decode.string)
    ("level" := Decode.int)
