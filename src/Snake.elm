module Snake exposing (..)

import Html exposing (..)

type alias Point = (Int, Int)
type Heading = North | East | South | West
type alias Cranium = { position : Point, heading: Heading }


-- MODEL


origin : Point
origin = (0, 0)


cranium : Cranium
cranium = Cranium origin East


move : Cranium -> Cranium
move the_cranium =
    let
        (x, y) = the_cranium.position
        (dx, dy) = 
            case the_cranium.heading of
                North ->
                    (0, -1)

                East ->
                    (1, 0)

                South ->
                    (0, 1)

                West ->
                    (-1, 0)
    in
        {the_cranium | position = (x + dx, y + dy)}


turn : Heading -> Cranium -> Cranium
turn heading the_cranium =
    if is_opposite_heading heading the_cranium.heading then
        the_cranium
    else
        {the_cranium | heading = heading}


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


main : Html msg
main =
    Html.text (toString cranium)