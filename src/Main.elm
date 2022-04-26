module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg as S
import Svg.Attributes as SA
import Array as A
import Maybe as M
import String exposing(fromInt)
import Random as R
import List as L
import Time as T
import Svg.Lazy

type Msg = Start | Stop | Generate | RandomCells (A.Array Cell) | Tick T.Posix | Step

type Status = Started | Stopped | Generated

type alias Model = {
        status : Status
        , board : Board
        , world : World
    }

type alias Board = {
    width : Int
    , height : Int
    }

type Cell = Dead | Alive

type alias World = {
    width : Int
    , height : Int
    , cells : (A.Array Cell)
    }

type alias Location = {
    x : Int
    , y : Int
    }

createWorld : Int -> Int -> World
createWorld width height = {
        width = width
        , height = height
        , cells = A.repeat (width * height) Dead
    }

calculateIndex : World -> Int -> Int -> Int
calculateIndex world x y = calculateIndexOfLocation world {x = x, y = y}

calculateLocation : World -> Int -> Location
calculateLocation world idx = 
    {
    x = modBy world.width idx
    , y = idx // world.width
    }

calculateIndexOfLocation : World -> Location -> Int
calculateIndexOfLocation world location =
    let norm = normalizeLocation world location
    in world.width * norm.y + norm.x

normalizeLocation : World -> Location -> Location
normalizeLocation world location =
    let
        normalizedX = location.x
            |> normalizeSize world.width
            |> normalizeNegative world.width
        normalizedY = location.y
            |> normalizeSize world.height
            |> normalizeNegative world.height
    in
        {x = normalizedX, y = normalizedY}

normalizeSize : Int -> Int -> Int
normalizeSize size coord = remainderBy size coord

normalizeNegative : Int -> Int -> Int
normalizeNegative size coord = 
    if coord < 0
    then size + coord
    else coord

withLivingAt : Int -> Int -> World -> World
withLivingAt x y world = 
    let
        idx = calculateIndex world x y
    in
        {world | cells = (A.set idx Alive world.cells)}

countLivingNeighbours : World -> Int -> Int -> Int
countLivingNeighbours world x y =
    getNeighbours world x y
    |> List.filter (\c -> c == Alive)
    |> List.length

getNeighbours : World -> Int -> Int -> (List Cell)
getNeighbours world x y = 
    let
        cellLoc = normalizeLocation world {x = x, y = y}
        getCell = getCellAt world
    in
        [
        getCell <| rightNeighbour world cellLoc 
        , getCell <| topRightNeighbour world cellLoc 
        , getCell <| topNeighbour world cellLoc 
        , getCell <| topLeftNeighbour world cellLoc
        , getCell <| leftNeighbour world cellLoc
        , getCell <| bottomLeftNeighbour world cellLoc
        , getCell <| bottomNeighbour world cellLoc
        , getCell <| bottomRightNeighbour world cellLoc
        ]

getCellAt : World -> Location -> Cell
getCellAt world loc = 
    M.withDefault 
        Dead 
        (A.get (calculateIndexOfLocation world loc) world.cells)

rightNeighbour : World -> Location -> Location
rightNeighbour world loc = normalizeLocation world {x = loc.x + 1, y = loc.y} 

topRightNeighbour : World -> Location -> Location
topRightNeighbour world loc = normalizeLocation world {x = loc.x + 1, y = loc.y - 1} 

topNeighbour : World -> Location -> Location
topNeighbour world loc = normalizeLocation world {x = loc.x, y = loc.y - 1} 

topLeftNeighbour : World -> Location -> Location
topLeftNeighbour world loc = normalizeLocation world {x = loc.x - 1, y = loc.y - 1} 

leftNeighbour : World -> Location -> Location
leftNeighbour world loc = normalizeLocation world {x = loc.x - 1, y = loc.y} 

bottomLeftNeighbour : World -> Location -> Location
bottomLeftNeighbour world loc = normalizeLocation world {x = loc.x - 1, y = loc.y + 1}

bottomNeighbour : World -> Location -> Location
bottomNeighbour world loc = normalizeLocation world {x = loc.x, y = loc.y + 1} 

bottomRightNeighbour : World -> Location -> Location
bottomRightNeighbour world loc = normalizeLocation world {x = loc.x + 1, y = loc.y + 1} 

nextGeneration : World -> World
nextGeneration currentGen = 
    let
        newWidth = currentGen.width
        newHeight = currentGen.height
        newCells = (nextGenerationCells currentGen)
    in
        {width = newWidth, height = newHeight, cells = newCells}

nextGenerationCells : World -> (A.Array Cell)
nextGenerationCells currentWorld =
    List.range 0 (A.length currentWorld.cells)
    |> List.map (fate currentWorld)
    |> A.fromList
    

fate : World -> Int -> Cell
fate world idx = 
    let
        loc = calculateLocation world idx
        count = countLivingNeighbours world loc.x loc.y
        cell = getCellAt world loc
    in
        if count == 3
        then Alive
        else
            if count == 2 && cell == Alive
            then Alive
            else Dead

-- main
main : Program () Model Msg
main = Browser.document {
    init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




-- init
init : () -> (Model, Cmd Msg)
init _ = 
    let
        xRatio = 16
        yRatio = 9
        resolutionMultiplier = 120
        marginMultipier = 20
        worldMultiplier = 10
    in
    (
        {
            status = Stopped
            , board = { width = (xRatio * (resolutionMultiplier - marginMultipier)), height = (yRatio * (resolutionMultiplier - marginMultipier))}
            , world = (createWorld (xRatio * worldMultiplier) (yRatio * worldMultiplier))
        }
        , Cmd.none
    )

-- view
view : Model -> Browser.Document Msg
view model = {
    title = "Game of Life"
    , body = viewBody model
    }

viewBoard : Model -> Html Msg
viewBoard model =
    S.svg 
        [ SA.width (fromInt model.board.width), SA.height (fromInt model.board.height)] 
        (L.append (viewWorld model.board model.world) [text "not supported"])

viewWorld : Board -> World -> List (S.Svg Msg)
viewWorld board world =
    let
        cWidth = cellWidth board world
        cHeight = cellHeight board world
    in
        List.range 0 (A.length world.cells)
        |> List.map (viewCellAtIndex world cWidth cHeight)


viewCellAtIndex : World -> Int -> Int -> Int -> S.Svg Msg
viewCellAtIndex world cWidth cHeight idx =
    let
        location = calculateLocation world idx
        cell = getCellAt world location
    in
        viewCell location cWidth cHeight cell

cellWidth : Board -> World -> Int
cellWidth board world = board.width // world.width

cellHeight : Board -> World -> Int
cellHeight board world = board.height // world.height

viewCell : Location -> Int -> Int -> Cell -> S.Svg Msg
viewCell location width height cell =
    let
        fillColor = case cell of
            Alive -> "rgb(0,0,0)"
            Dead -> "rgb(255,255,255)"
    in
        S.rect 
            [
            SA.x <| fromInt <| width * location.x
            , SA.y <| fromInt <| height * location.y
            , SA.width <| fromInt <| width
            , SA.height <| fromInt <| height
            , Attr.style "fill" fillColor
            , Attr.style "stroke-width" "1"
            , Attr.style "stroke" "rgb(0,0,0)"
            ]
            []

viewBody : Model -> List (Html Msg)
viewBody model =
    let
        widthStr = String.fromInt model.board.width
    in
    [
        div 
            [
            Attr.style "margin-left" "auto"
            , Attr.style "margin-right" "auto"
            , Attr.style "margin-bottom" ".5rem"
            , Attr.style "margin-top" ".2rem"
            , Attr.style "width" (String.append widthStr "px")
            ]
            [
            button [Attr.style "margin-right" ".2rem", onClick Generate] [text "Randomize"]
            , button [Attr.style "margin-right" ".2rem", onClick Start] [text "Start"]
            , button [Attr.style "margin-right" ".2rem", onClick Step] [text "Step"]
            , button [onClick Stop] [text "Stop"]
            ]
        , div 
            [
            Attr.style "margin-left" "auto"
            , Attr.style "margin-right" "auto"
            , Attr.style "width" (String.append widthStr "px")
            ] 
            [Svg.Lazy.lazy viewBoard model]
    ]

-- update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start -> 
            (
                {model | status = Started}
                , Cmd.none
            )
        Stop -> 
            (
                if model.status == Started
                then {model | status = Stopped}
                else model
                , Cmd.none
            )
        Generate ->
            (
                model
                , R.generate RandomCells (generateRandomCells (model.world.width*model.world.height))
            )
        RandomCells newCells ->
            (
                {
                    status = Generated
                    , world = 
                        {
                            width = model.world.width
                            , height = model.world.height
                            , cells = newCells
                        }
                    , board = model.board
                }
                , Cmd.none
            )
        Tick _ ->
            (
                if model.status == Started
                then {model | world = (nextGeneration model.world)}
                else model
                , Cmd.none
            )
        Step ->
            (
                if model.status == Started
                then {model | status = Stopped}
                else {model | status = Stopped, world = (nextGeneration model.world)}
                , Cmd.none
            )

generateRandomCells : Int -> R.Generator (A.Array Cell)
generateRandomCells n =
    R.list n (R.weighted (20, Alive) [(80, Dead)])
    |> R.map A.fromList


-- subscriptions
subscriptions : Model -> Sub Msg
subscriptions model = T.every (1000 / 6) Tick

-- helpers
printStatus : Model -> String
printStatus model =
    case model.status of
        Started -> "Started"
        Stopped -> "Stopped"
        Generated -> "Generated"
