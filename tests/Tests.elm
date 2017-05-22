module Tests exposing (..)

import Test exposing (..)
import Expect
import Tuple exposing (..)
import Snake


all : Test
all =
    describe "Model"
        [ test "Cranium starts at origin" <|
            \() ->
                Expect.equal Snake.origin (.position Snake.cranium)
        , test "Cranium start moving East" <|
            \() ->
                Expect.equal Snake.East (.heading Snake.cranium)
        , test "Move makes position == origin + (0, 1)" <|
            \() ->
                Expect.equal (first Snake.origin + 1, second Snake.origin) (.position (Snake.move Snake.cranium))
        , test "Move doesn't change the heading" <|
            \() ->
                Expect.equal Snake.East (.heading (Snake.move Snake.cranium))
        , test "North is the opposite of South" <|
            \() ->
                [ Expect.equal Snake.North (Snake.opposite_heading Snake.South)
                , Expect.equal Snake.South (Snake.opposite_heading Snake.North)
                ]
        --, test "Turn North changes the heading to North" <|
        --    \() ->
        --        Expect.equal Snake.North (.heading (Snake.turn_north Snake.cranium))

        ]
