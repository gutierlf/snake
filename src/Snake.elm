module Snake exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Time exposing (..)
import Keyboard


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
type alias Head = Point
type Heading = North | East | South | West
type alias Length = Int
type alias Model =
    { state : State
    , tracks : List Point
    , heading: Heading
    , length : Length
    }


(boardWidth,boardHeight) = (6,4)
(halfWidth,halfHeight) = (3,2)
spacebar : Keyboard.KeyCode
spacebar = 32


init : (Model, Cmd Msg)
init =
    let
        length = 2
        origin = (halfWidth, halfHeight)
        y = List.repeat length halfHeight
        dx = List.range 0 (length - 1)
        x = List.map2 (-) (List.repeat length halfWidth) dx
        tracks = List.map2 (,) x y
        initialModel = Model Reset tracks East length
    in
        (initialModel, Cmd.none)


-- UPDATE


type Msg
    = Tick Time
    | KeyMsg Keyboard.KeyCode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ ->
            (tick model, Cmd.none)

        KeyMsg code ->
            (applyKeyInput code model, Cmd.none)


applyKeyInput : Keyboard.KeyCode -> Model -> Model
applyKeyInput code model =
    if code == spacebar then
        toggleState model
    else
        model


toggleState : Model -> Model
toggleState model =
    let
        state =
            case model.state of
                Reset -> Play
                Play  -> Pause
                Pause -> Play
                Over  -> Reset
    in
        { model | state = state }


tick : Model -> Model
tick model =
    let
        head = advanceHead model
        tracks = head :: model.tracks
    in
        { model | tracks = tracks } 


advanceHead : Model -> Head
advanceHead model =
    let
        (x, y) = 
            case List.head model.tracks of
                Nothing    -> (0, 0)
                Just point -> point
        (dx, dy) = 
            case model.heading of
                North -> ( 0, -1)
                East  -> ( 1,  0)
                South -> ( 0,  1)
                West  -> (-1,  0)
    in
        (x + dx, y + dy)


turn : Heading -> Model -> Model
turn heading model =
    if canTurn model heading then
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
        North -> South
        East  -> West
        South -> North
        West  -> East


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keyboard = Keyboard.downs KeyMsg
    in
        case model.state of
            Reset ->
                keyboard

            Play ->
                Sub.batch
                    [ keyboard
                    , Time.every second Tick
                    ]

            Pause ->
                keyboard

            Over ->
                keyboard


-- VIEW


view : Model -> Html Msg
view model =
    text (toString model)
