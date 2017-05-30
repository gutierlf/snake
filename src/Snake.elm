module Snake exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Time exposing (..)
import Keyboard
import Array
import Random


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
    , food : Point
    }


(boardWidth,boardHeight) = (30,20)


initialModel : Model
initialModel =
    let
        length = 10
        halfHeight = boardHeight // 2 
        halfWidth = boardWidth // 2
        y = List.repeat length halfHeight
        dx = List.range 0 (length - 1)
        x = List.map2 (-) (List.repeat length halfWidth) dx
        tracks = List.map2 (,) x y
        food = (halfWidth + 2, halfHeight)
    in
        Model Reset tracks East length food


init : (Model, Cmd Msg)
init =
    (initialModel, generateRandomFood)


-- COMMANDS


generateRandomFood : Cmd Msg
generateRandomFood = 
    Random.generate NewFood randomPoint


randomPoint : Random.Generator (Int,Int)
randomPoint =
    Random.pair (Random.int 1 boardWidth) (Random.int 1 boardHeight)


-- UPDATE


type Msg
    = Tick Time
    | KeyMsg Keyboard.KeyCode
    | NewFood Point


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ ->
            tick model

        KeyMsg code ->
            applyKeyInput code model

        NewFood food ->
            ( { model | food = food }, Cmd.none)


applyKeyInput : Keyboard.KeyCode -> Model -> (Model, Cmd Msg)
applyKeyInput code model =
    let
        spacebar    = 32
        left_arrow  = 37
        up_arrow    = 38
        right_arrow = 39
        down_arrow  = 40

        turn_with_no_command heading model = (turn heading model, Cmd.none)
    in
        if code == spacebar then
            toggleState model
        else if model.state == Play then
            if code == left_arrow then
                turn_with_no_command West model
            else if code == up_arrow then
                turn_with_no_command North model
            else if code == right_arrow then
                turn_with_no_command East model
            else if code == down_arrow then
                turn_with_no_command South model
            else
                (model, Cmd.none)
        else
            (model, Cmd.none)


toggleState : Model -> (Model, Cmd Msg)
toggleState model =
    case model.state of
        Reset -> ({ model | state = Play }, generateRandomFood)
        Play  -> ({ model | state = Pause }, Cmd.none)
        Pause -> ({ model | state = Play }, Cmd.none)
        Over  -> (initialModel, Cmd.none)


tick : Model -> (Model, Cmd Msg)
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
        (length, command) =
            if encountered head model.food then
                eat model
            else
                (model.length, Cmd.none)
    in
        ({ model | tracks = tracks, state = state, length = length }, command)


encountered : Head -> Point -> Bool
encountered head food =
    head == food


eat : Model -> (Length, Cmd Msg)
eat model =
    (model.length + 3, generateRandomFood)


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
                    , Time.every (250 * Time.millisecond) Tick
                    ]

            Pause ->
                keyboard

            Over ->
                keyboard


-- VIEW


type Object = Wall | SnakeHead | SnakeBody | Food | None


board : Model -> List (List Object)
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

        snakeHead = getHeadFrom model.tracks
        snakeBody = getBodyFrom model.tracks model.length

        boardX : List (List Int)
        boardX =
            boardWidth
            |> List.range 0
            |> List.repeat (boardHeight + 1)
        boardY : List (List Int)
        boardY =
            boardHeight
            |> List.range 0
            |> List.map (List.repeat (boardWidth + 1))

        boardXY : List (List Point)
        boardXY = List.map2 (List.map2 (,)) boardX boardY

        setObject point =
            if point == snakeHead then
                SnakeHead
            else if List.member point snakeBody then
                SnakeBody
            else if List.member point walls then
                Wall
            else if point == model.food then
                Food
            else
                None
    in
        List.map (List.map setObject) boardXY


objectStrings : Object -> Char
objectStrings object =
    case object of
        Wall -> '+'
        SnakeHead -> 'x'
        SnakeBody -> 'o'
        Food -> '*'
        None -> '.'


boardStrings : List (List Object) -> List String
boardStrings board = 
    let
        f : List Object -> String
        f objects =
            objects
            |> List.map objectStrings
            |> String.fromList
    in
        List.map f board


viewBoardLine : String -> Html Msg
viewBoardLine line =
    p [] [text line]


viewBoardStrings : Model -> List (Html Msg)
viewBoardStrings model =
    let
        strings = model |> board |> boardStrings  
    in
        List.map viewBoardLine strings


view : Model -> Html Msg
view model =
    div [class "board"] (viewBoardStrings model)
