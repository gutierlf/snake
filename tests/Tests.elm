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
                Expect.equal Snake.origin (.position Snake.initialModel)
        , test "Cranium start moving East" <|
            \() ->
                Expect.equal Snake.East (.heading Snake.initialModel)
        , test "Move makes position == origin + (0, 1)" <|
            \() ->
                Expect.equal (first Snake.origin + 1, second Snake.origin) (.position (Snake.move Snake.initialModel))
        , test "Move doesn't change the heading" <|
            \() ->
                Expect.equal Snake.East (.heading (Snake.move Snake.initialModel))
        , test "North is the opposite of South" <|
            \() ->
                Expect.equal Snake.North (Snake.opposite_heading Snake.South)
        , test "South is the opposite of North" <|
            \() ->
                Expect.equal Snake.South (Snake.opposite_heading Snake.North)
        , test "East is the opposite of West" <|
            \() ->
                Expect.equal Snake.East (Snake.opposite_heading Snake.West)
        , test "West is the opposite of East" <|
            \() ->
                Expect.equal Snake.West (Snake.opposite_heading Snake.East)
        , test "opposite_heading?" <|
            \() ->
                Expect.true "Expected North to be opposite of South" (Snake.is_opposite_heading Snake.North Snake.South)
        , test "Turn North changes the heading to North" <|
            \() ->
                Expect.equal Snake.North (.heading (Snake.turn Snake.North Snake.initialModel))
        , test "Turn East changes the heading to East" <|
            \() ->
                Expect.equal Snake.East (.heading (Snake.turn Snake.East Snake.initialModel))
        , test "Turn South changes the heading to South" <|
            \() ->
                Expect.equal Snake.South (.heading (Snake.turn Snake.South Snake.initialModel))
        , test "Turn West leaves heading at East" <|
            \() ->
                Expect.equal Snake.East (.heading (Snake.turn Snake.West Snake.initialModel))

        ]
