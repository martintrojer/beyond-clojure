module Main (..) where

import Html exposing (..)
import Task
import StartApp
import Effects exposing (Effects, Never)
import Actions exposing (..)
import Models exposing (..)
import Update exposing (..)
import View exposing (..)
import Mailboxes exposing (..)
import Routing
import Players.Effects
import Players.Actions


init : ( AppModel, Effects Action )
init =
  let
    fxs =
      [ Effects.map PlayersAction Players.Effects.fetchAll ]

    fx =
      Effects.batch fxs
  in
    ( initialModel, fx )


routerSignal : Signal Action
routerSignal =
  Signal.map RoutingAction Routing.signal


app : StartApp.App AppModel
app =
  StartApp.start
    { init = init
    , inputs = [ routerSignal, actionsMailbox.signal, getDeleteConfirmationSignal ]
    , update = update
    , view = view
    }


getDeleteConfirmationSignal : Signal Actions.Action
getDeleteConfirmationSignal =
  let
    toAction id =
      id
        |> Players.Actions.DeletePlayer
        |> PlayersAction
  in
    Signal.map toAction getDeleteConfirmation


main : Signal.Signal Html
main =
  app.html


port routeRunTask : Task.Task () ()
port routeRunTask =
  Routing.run


port askDeleteConfirmation : Signal ( Int, String )
port askDeleteConfirmation =
  askDeleteConfirmationMailbox.signal


port getDeleteConfirmation : Signal Int


port runner : Signal (Task.Task Never ())
port runner =
  app.tasks
