module Snake exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Time exposing (..)
import Keyboard
import Array


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
type alias Tracks = List Point
type alias Body = List Point
type Heading = North | East | South | West
type alias Length = Int
type alias Model =
    { state : State
    , tracks : Tracks
    , heading: Heading
    , length : Length
    }


(boardWidth,boardHeight) = (6,4)


initialModel : Model
initialModel =
    let
        length = 2
        y = List.repeat length (boardHeight // 2)
        dx = List.range 0 (length - 1)
        x = List.map2 (-) (List.repeat length (boardWidth // 2)) dx
        tracks = List.map2 (,) x y
    in
        Model Reset tracks East length


init : (Model, Cmd Msg)
init =
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
    let
        spacebar    = 32
        left_arrow  = 37
        up_arrow    = 38
        right_arrow = 39
        down_arrow  = 40
    in
        if code == spacebar then
            toggleState model
        else if model.state == Play then
            if code == left_arrow then
                turn West model
            else if code == up_arrow then
                turn North model
            else if code == right_arrow then
                turn East model
            else if code == down_arrow then
                turn South model
            else
                model
        else
            model


toggleState : Model -> Model
toggleState model =
    case model.state of
        Reset -> { model | state = Play }
        Play  -> { model | state = Pause }
        Pause -> { model | state = Play }
        Over  -> initialModel


tick : Model -> Model
tick model =
    let
        head = advanceHead model
        tracks = head :: model.tracks
        body = getBodyFrom tracks model.length
        state =
            if anyCollisions head body then
                Over
            else
                model.state
    in
        { model | tracks = tracks, state = state }


advanceHead : Model -> Head
advanceHead model =
    let
        (x, y) = getHeadFrom model.tracks
        (dx, dy) = 
            case model.heading of
                North -> ( 0, -1)
                East  -> ( 1,  0)
                South -> ( 0,  1)
                West  -> (-1,  0)
    in
        (x + dx, y + dy)


getHeadFrom : Tracks -> Head
getHeadFrom tracks =
    case List.head tracks of
        Nothing   -> (0, 0)
        Just head -> head


getBodyFrom : Tracks -> Length -> Body
getBodyFrom tracks length =
    List.drop 1 (List.take length tracks)


anyCollisions : Head -> Body -> Bool
anyCollisions head body =
    (head |> collidedWithWall) || (collidedwith head body)


collidedWithWall : Head -> Bool
collidedWithWall (x, y) =
    x == 0 || x == boardWidth || y == 0 || y == boardHeight


collidedwith : Head -> Body -> Bool
collidedwith head body =
    List.member head body


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


type Object = Wall | Snake | None


board : Model -> Array.Array (Array.Array Object)
board model = 
    let
        horizontalWallX = List.range 0 boardWidth
        horizontalWallY = List.repeat boardWidth
        verticalWallX   = List.repeat boardHeight
        verticalWallY   = List.range 0 boardHeight

        horizontalWall y = List.map2 (,) horizontalWallX   (horizontalWallY y)
        verticalWall x   = List.map2 (,) (verticalWallX x) verticalWallY

        topWall    = horizontalWall 0
        bottomWall = horizontalWall boardHeight
        leftWall   = verticalWall 0
        rightWall  = verticalWall boardWidth

        walls = List.concat [topWall, bottomWall, leftWall, rightWall]

        snake = (getHeadFrom model.tracks) :: (getBodyFrom model.tracks model.length)

        boardX : Array.Array (Array.Array Int)
        boardX =
            boardWidth
            |> List.range 0
            |> Array.fromList
            |> Array.repeat (boardHeight + 1)
        boardY : Array.Array (Array.Array Int)
        boardY =
            Array.initialize (boardHeight + 1) (\n -> Array.repeat (boardWidth + 1) n)


        arrayMap2 : (a -> b -> result) -> Array.Array a -> Array.Array b -> Array.Array result
        arrayMap2 f a b =
            let
                listA = Array.toList a
                listB = Array.toList b
                mapped = List.map2 f listA listB 
            in
                Array.fromList mapped  

        boardXY : Array.Array (Array.Array Point)
        boardXY = arrayMap2 (arrayMap2 (,)) boardX boardY

        setObject point =
            if List.member point snake then
                Snake
            else if List.member point walls then
                Wall
            else
                None
    in
        Array.map (Array.map setObject) boardXY


view : Model -> Html Msg
view model =
    div []
    [ text (toString (board model))
    , text (toString model) 
    ]
