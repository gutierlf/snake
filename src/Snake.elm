module Snake exposing (..)


type alias Point = (Int, Int)
type Heading = North | East | South | West
type alias Cranium = { position : Point, heading: Heading }


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


--turn_north : Cranium -> Cranium
--turn_north the_cranium =
--    let
--        heading =
--            case the_cranium.heading of
--                North | East | West ->
--                    North

--                South ->
--                    South
--    in
--        {the_cranium | heading = heading}
            

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