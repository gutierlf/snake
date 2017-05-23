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


cranium : Model
cranium = Model origin East


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


type Msg = Tick

update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            move model


-- VIEW


view : Model -> Html Msg
view model =
    div [ ]
        [ div [ ]
            [ button [ onClick Tick ] [ text "Tick" ] ]
        , div [ ] [ text (toString model) ]
        ]



main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = cranium
        , view = view
        , update = update
        }

