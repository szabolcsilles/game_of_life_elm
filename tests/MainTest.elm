module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Array exposing (empty)

import Main exposing (
    createWorld
    , calculateIndex
    , withLivingAt
    , countLivingNeighbours
    , fate
    , calculateLocation
    , Cell(..)
    , Location
    )

exampleTests : Test
exampleTests =
    describe "Addition"
        [
        test 
            "two plus two equals four" <| 
            \_ -> (2 + 2) |> Expect.equal 4

        , test 
            "three plus four equals seven" <|
            \_ -> (3 + 4) |> Expect.equal 7
        ]

worldTests : Test
worldTests =
    describe "World"
    [
    test
        "calculateIndex should return the correct index when coordinates between boundaries " <|
        \_ -> 
            (calculateIndex (createWorld 4 3) 1 1) 
            |> Expect.equal 5

    , test
        "calculateIndex should return the correct index when negative coordinates given" <|
        \_ -> 
            (calculateIndex (createWorld 3 2) -2 -1) 
            |> Expect.equal 4

    , test
        "calculateIndex should return the correct index when large coordinates given" <|
        \_ ->
            (calculateIndex (createWorld 5 4) -11 10)
            |> Expect.equal 14
    , test
        "calculateLocation should return the location from index position" <|
        \_ ->
            (calculateLocation (createWorld 6 5) 7)
            |> Expect.equal {x = 1, y = 1}
    ]

neigboursOfDeadTests : Test
neigboursOfDeadTests =
    describe "Neighbours of dead cell"
    [
    test
        "fate should return Dead when no living neigbours" <|
        \_ ->
            let
                noNeighbours = createWorld 3 3
            in
                fate noNeighbours 4
            |> Expect.equal Dead

    , test
        "fate should return Dead when one living neigbours" <|
        \_ ->
            let
                oneNeighbours = createWorld 3 3
                    |> withLivingAt 1 2
            in
                fate oneNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Dead when two living neigbours" <|
        \_ ->
            let
                twoNeighbours = createWorld 3 3
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
            in
                fate twoNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Alive when three living neigbours" <|
        \_ ->
            let
                threeNeighbours = createWorld 3 3
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
            in
                fate threeNeighbours 4
            |> Expect.equal Alive
    , test
        "fate should return Dead when four living neigbours" <|
        \_ ->
            let
                fourNeighbours = createWorld 3 3
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
                    |> withLivingAt 2 1
            in
                fate fourNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Dead when five living neigbours" <|
        \_ ->
            let
                fiveNeighbours = createWorld 3 3
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
                    |> withLivingAt 2 1
                    |> withLivingAt 2 2
            in
                fate fiveNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Dead when six living neigbours" <|
        \_ ->
            let
                sixNeighbours = createWorld 3 3
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
                    |> withLivingAt 2 1
                    |> withLivingAt 2 2
                    |> withLivingAt 1 2
            in
                fate sixNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Dead when seven living neigbours" <|
        \_ ->
            let
                sevenNeighbours = createWorld 3 3
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
                    |> withLivingAt 2 1
                    |> withLivingAt 2 2
                    |> withLivingAt 1 2
                    |> withLivingAt 0 2
            in
                fate sevenNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Dead when eight living neigbours" <|
        \_ ->
            let
                eightNeighbours = createWorld 3 3
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
                    |> withLivingAt 2 1
                    |> withLivingAt 2 2
                    |> withLivingAt 1 2
                    |> withLivingAt 0 2
                    |> withLivingAt 0 1
            in
                fate eightNeighbours 4
            |> Expect.equal Dead
    ]

neighboursOfLivingTests : Test
neighboursOfLivingTests =
    describe "Neighbours of living cell"
    [
    test
        "countLivingNeighbours should return the number of living neighbours" <|
        \_ ->
            let
                twoNeigbours = createWorld 3 3
                    |> withLivingAt 1 1
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
            in
                countLivingNeighbours twoNeigbours 1 1
        |> Expect.equal 2

    , test
        "fate should return Dead when no living neigbours" <|
        \_ ->
            let
                noNeighbours = createWorld 3 3
                    |> withLivingAt 1 1
            in
                fate noNeighbours 4
            |> Expect.equal Dead

    , test
        "fate should return Dead when one living neigbours" <|
        \_ ->
            let
                oneNeighbours = createWorld 3 3
                    |> withLivingAt 1 1
                    |> withLivingAt 1 2
            in
                fate oneNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Alive when two living neigbours" <|
        \_ ->
            let
                twoNeighbours = createWorld 3 3
                    |> withLivingAt 1 1
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
            in
                fate twoNeighbours 4
            |> Expect.equal Alive
    , test
        "fate should return Alive when three living neigbours" <|
        \_ ->
            let
                threeNeighbours = createWorld 3 3
                    |> withLivingAt 1 1
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
            in
                fate threeNeighbours 4
            |> Expect.equal Alive
    , test
        "fate should return Dead when four living neigbours" <|
        \_ ->
            let
                fourNeighbours = createWorld 3 3
                    |> withLivingAt 1 1
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
                    |> withLivingAt 2 1
            in
                fate fourNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Dead when five living neigbours" <|
        \_ ->
            let
                fiveNeighbours = createWorld 3 3
                    |> withLivingAt 1 1
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
                    |> withLivingAt 2 1
                    |> withLivingAt 2 2
            in
                fate fiveNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Dead when six living neigbours" <|
        \_ ->
            let
                sixNeighbours = createWorld 3 3
                    |> withLivingAt 1 1
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
                    |> withLivingAt 2 1
                    |> withLivingAt 2 2
                    |> withLivingAt 1 2
            in
                fate sixNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Dead when seven living neigbours" <|
        \_ ->
            let
                sevenNeighbours = createWorld 3 3
                    |> withLivingAt 1 1
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
                    |> withLivingAt 2 1
                    |> withLivingAt 2 2
                    |> withLivingAt 1 2
                    |> withLivingAt 0 2
            in
                fate sevenNeighbours 4
            |> Expect.equal Dead
    , test
        "fate should return Dead when eight living neigbours" <|
        \_ ->
            let
                eightNeighbours = createWorld 3 3
                    |> withLivingAt 1 1
                    |> withLivingAt 0 0
                    |> withLivingAt 1 0
                    |> withLivingAt 2 0
                    |> withLivingAt 2 1
                    |> withLivingAt 2 2
                    |> withLivingAt 1 2
                    |> withLivingAt 0 2
                    |> withLivingAt 0 1
            in
                fate eightNeighbours 4
            |> Expect.equal Dead
    ]