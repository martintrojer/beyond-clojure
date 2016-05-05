module Routing (..) where

import Task exposing (Task)
import Effects exposing (Effects, Never)
import Hop
import Hop.Types exposing (Location, PathMatcher, Router, newLocation)
import Hop.Navigate exposing (navigateTo)
import Hop.Matchers exposing (match1, match2, match3, int)
import Players.Models exposing (PlayerId)


type Route
  = PlayersRoute
  | PlayerEditRoute PlayerId
  | NotFoundRoute


type
  Action
  -- Wrap Hop Task
  = HopAction ()
    -- Location changed in brower
  | ApplyRoute ( Route, Location )
    -- Us affecting a location change
  | NavigateTo String


type alias Model =
  { location : Location
  , route : Route
  }


initialModel : Model
initialModel =
  { location = newLocation
  , route = PlayersRoute
  }


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NavigateTo path ->
      ( model, Effects.map HopAction (navigateTo path) )

    ApplyRoute ( route, location ) ->
      ( { model | route = route, location = location }, Effects.none )

    HopAction () ->
      ( model, Effects.none )



-- Route matchers
-- "/players" --> PlayersRoute


indexMatcher : PathMatcher Route
indexMatcher =
  match1 PlayersRoute "/"


playersMatcher : PathMatcher Route
playersMatcher =
  match1 PlayersRoute "/players"


playerEditMatch : PathMatcher Route
playerEditMatch =
  match3 PlayerEditRoute "/players/" int "/edit"


matchers : List (PathMatcher Route)
matchers =
  [ indexMatcher
  , playersMatcher
  , playerEditMatch
  ]


router : Router Route
router =
  Hop.new
    { matchers = matchers
    , notFound = NotFoundRoute
    }


run : Task () ()
run =
  router.run


signal : Signal Action
signal =
  Signal.map ApplyRoute router.signal
