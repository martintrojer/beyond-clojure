module Players.Update (..) where

import Effects exposing (Effects)
import Players.Actions exposing (..)
import Players.Effects exposing (..)
import Players.Models exposing (..)
import Task
import Hop.Navigate exposing (navigateTo)
import Debug exposing (log)


type alias UpdateModel =
  { players : List Player
  , showErrorAddress : Signal.Address String
  , deleteConfirmationAddress : Signal.Address ( PlayerId, String )
  }


update : Action -> UpdateModel -> ( List Player, Effects Action )
update action model =
  case action of
    EditPlayer id ->
      let
        path =
          "/players/" ++ (toString id) ++ "/edit"
      in
        ( model.players, Effects.map HopAction (navigateTo path) )

    ListPlayers ->
      ( model.players, Effects.map HopAction (navigateTo "/players") )

    HopAction _ ->
      ( model.players, Effects.none )

    FetchAllDone res ->
      case res of
        Ok players ->
          ( players, Effects.none )

        Err error ->
          let
            errorMessage =
              toString error

            fx =
              Signal.send model.showErrorAddress errorMessage
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.players, fx )

    CreatePlayer ->
      ( model.players, createPlayer new )

    CreatePlayerDone result ->
      case result of
        Ok player ->
          let
            updatedCollection =
              player :: model.players

            fx =
              Task.succeed (EditPlayer player.id)
                |> Effects.task
          in
            ( updatedCollection, fx )

        Err error ->
          let
            fx =
              Signal.send model.showErrorAddress (toString error)
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.players, fx )

    ChangeName playerId newName ->
      let
        fxForPlayer player =
          if player.id /= playerId then
            Effects.none
          else
            let
              updatedPlayer =
                { player | name = newName }
            in
              updatePlayer updatedPlayer

        fx =
          List.map fxForPlayer model.players
            |> Effects.batch
      in
        ( model.players, fx )

    ChangeLevel playerId howMuch ->
      let
        fxForPlayer player =
          if player.id /= playerId then
            Effects.none
          else if player.level + howMuch <= 0 then
            Effects.none
          else
            let
              updatedPlayer =
                { player | level = player.level + howMuch }
            in
              updatePlayer updatedPlayer

        fx =
          List.map fxForPlayer model.players
            |> Effects.batch
      in
        ( model.players, fx )

    SaveDone result ->
      case result of
        Ok player ->
          let
            updatedCollection =
              List.map
                (\p ->
                  if p.id == player.id then
                    player
                  else
                    p
                )
                model.players
          in
            ( updatedCollection, Effects.none )

        Err error ->
          let
            fx =
              Signal.send model.showErrorAddress (toString error)
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.players, fx )

    DeletePlayerIntent player ->
      let
        msg =
          "Are you sure you want to delete " ++ player.name ++ "?"

        fx =
          Signal.send model.deleteConfirmationAddress ( player.id, msg )
            |> Effects.task
            |> Effects.map TaskDone
      in
        ( model.players, fx )

    DeletePlayer playerId ->
      ( model.players, deletePlayer playerId )

    DeletePlayerDone playerId result ->
      case result of
        Ok _ ->
          let
            newPlayers =
              List.filter (\p -> p.id /= playerId) model.players
          in
            ( newPlayers, Effects.none )

        Err error ->
          let
            fx =
              Signal.send model.showErrorAddress (toString error)
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.players, fx )

    TaskDone () ->
      ( model.players, Effects.none )

    NoOp ->
      ( model.players, Effects.none )
