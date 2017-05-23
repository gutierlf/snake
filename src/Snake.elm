module Snake exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Time exposing (..)

type alias Point = (Int, Int)
type Heading = North | East | South | West
type alias Model = { position : Point, heading: Heading }


-- MODEL


origin : Point
origin = (0, 0)


move : Model -> Model
move model =
    let
        (x, y) = model.position
        (dx, dy) = 
            case model.heading of
                North ->
                    (0, -1)

                East ->
                    (1, 0)

                South ->
                    (0, 1)

                West ->
                    (-1, 0)
    in
        {model | position = (x + dx, y + dy)}


turn : Heading -> Model -> Model
turn heading model =
    if is_opposite_heading heading model.heading then
        model
    else
        {model | heading = heading}


opposite_heading : Heading -> Heading
opposite_heading heading =
    case heading of
        North ->
            South

        East ->
            West

        South ->
            North

        West ->
            East

is_opposite_heading : Heading -> Heading -> Bool
is_opposite_heading heading other =
    opposite_heading heading == other


-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            (move model, Cmd.none)


initialModel : Model
initialModel = Model origin East


init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.none)


subs : Model -> Sub Msg
subs model =
  Time.every second Tick


-- VIEW


view : Model -> Html Msg
view model =
    text (toString model)



main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }