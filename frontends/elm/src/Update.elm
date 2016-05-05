module Update (..) where

import Effects exposing (Effects, Never)
import Actions exposing (..)
import Models exposing (..)
import Mailboxes exposing (..)
import Routing
import Players.Update


update : Action -> AppModel -> ( AppModel, Effects Action )
update action model =
  case action of
    PlayersAction action' ->
      let
        updateModel =
          { players = model.players
          , showErrorAddress = Signal.forwardTo actionsMailbox.address ShowError
          , deleteConfirmationAddress = askDeleteConfirmationMailbox.address
          }

        ( updatedPlayers, fx ) =
          Players.Update.update action' updateModel
      in
        ( { model | players = updatedPlayers }, Effects.map PlayersAction fx )

    RoutingAction action' ->
      let
        ( updatedRouting, fx ) =
          Routing.update action' model.routing
      in
        ( { model | routing = updatedRouting }, Effects.map RoutingAction fx )

    ShowError error ->
      ( { model | errorMessage = error }, Effects.none )

    NoOp ->
      ( model, Effects.none )
