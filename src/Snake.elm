module Snake exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Time exposing (..)


main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL


type State = Reset | Play | Pause | Over
type alias Point = (Int, Int)
type Heading = North | East | South | West
type alias Model =
    { state : State
    , positions : List Point
    , heading: Heading
    , length : Int
    }


(boardWidth,boardHeight) = (6,4)
(halfWidth,halfHeight) = (3,2)



init : (Model, Cmd Msg)
init =
    let
        length = 2
        origin = (halfWidth, halfHeight)
        y = List.repeat length halfHeight
        dx = List.range 0 (length - 1)
        x = List.map2 (-) (List.repeat length halfWidth) dx
        positions = List.map2 (,) x y
        initialModel = Model Reset positions East length
    in
        (initialModel, Cmd.none)


-- UPDATE


type Msg
    = Tick Time
    | TogglePause


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            (move model, Cmd.none)

        TogglePause ->
            let
                state =
                    case model.state of
                        Reset -> Play
                        Play  -> Pause
                        Pause -> Play
                        Over  -> Reset
            in
                ({ model | state = state }, Cmd.none)


move : Model -> Model
move model =
    let
        (x, y) = 
            case List.head model.positions of
                Nothing ->
                    (0, 0)

                Just point ->
                   point 
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
        { model | positions = (x + dx, y + dy) :: model.positions }


turn : Heading -> Model -> Model
turn heading model =
    if canTurn model heading
    then
        {model | heading = heading}
    else
        model


canTurn : Model -> Heading -> Bool
canTurn model heading = 
    not (is_opposite_heading model.heading heading)


is_opposite_heading : Heading -> Heading -> Bool
is_opposite_heading heading other =
    opposite_heading heading == other


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


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Reset ->
            Sub.none

        Play ->
            Time.every second Tick

        Pause ->
            Sub.none

        Over ->
            Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick TogglePause ] [ text "Play" ]
        , br [] []
        , text (toString model)
        ]
