module Room exposing (Candidates, Coordinate, CoordinateStatus(..), Direction(..), Point, Room, generateRoom, initializeRoom)

import List.Extra as ListE


type alias Room =
    { points : RoomPoints
    , maxOfCoordinate : Int
    , stairsCoordinate : Coordinate
    , startCoordinate : Coordinate
    , directions : List Direction
    , candidates : Candidates
    }


type alias RoomPoints =
    List Point


type alias Point =
    { coordinate : Coordinate
    , coordinateStatus : CoordinateStatus
    , possibleDirections : List Direction
    }


type alias Candidates =
    List Point


type alias Coordinate =
    { x : Int
    , y : Int
    }


type CoordinateStatus
    = Wall
    | Road


type Direction
    = UP
    | DOWN
    | RIGHT
    | LEFT


initializeRoom : Int -> Room
initializeRoom maxOfCoordinate =
    let
        points =
            maxOfCoordinate
                |> List.range 0
                |> initializePoints maxOfCoordinate
    in
    { points = points
    , maxOfCoordinate = maxOfCoordinate
    , stairsCoordinate = Coordinate 0 0
    , startCoordinate = Coordinate 0 0
    , directions = []
    , candidates = initializeCandidate points
    }


initializePoints : Int -> List Int -> RoomPoints
initializePoints maxOfCoordinate width =
    ListE.lift2 (initializePoint maxOfCoordinate) width width


initializePoint : Int -> Int -> Int -> Point
initializePoint maxOfCoordinate y x =
    Point (Coordinate x y)
        (if isPerimeter maxOfCoordinate x y then
            Road

         else
            Wall
        )
        [ UP, DOWN, RIGHT, LEFT ]


isPerimeter : Int -> Int -> Int -> Bool
isPerimeter maxOfCoordinate x y =
    x == 0 || y == 0 || x == maxOfCoordinate || y == maxOfCoordinate


isOdd : Point -> Bool
isOdd point =
    let
        modBy2 =
            modBy 2
    in
    modBy2 point.coordinate.x == 1 && modBy2 point.coordinate.y == 1


initializeCandidate : RoomPoints -> Candidates
initializeCandidate roomPoints =
    List.filter (\p -> isOdd p) roomPoints



-- generate room


makeWall : Int -> RoomPoints -> RoomPoints
makeWall numOfPint roomPoints =
    List.map
        (\p ->
            if isPerimeter numOfPint p.coordinate.x p.coordinate.y then
                { p | coordinateStatus = Wall }

            else
                p
        )
        roomPoints


generateRoom : Room -> Room
generateRoom room =
    let
        direction =
            Maybe.withDefault UP (List.head room.directions)

        newDirections =
            case room.directions of
                d :: ds ->
                    ds ++ [ d ]

                _ ->
                    room.directions
    in
    case room.candidates of
        [] ->
            breakThroughTheWall { room | points = makeWall room.maxOfCoordinate room.points }

        point :: points ->
            let
                ( c2, c1 ) =
                    case direction of
                        UP ->
                            ( Coordinate point.coordinate.x (point.coordinate.y + 2)
                            , Coordinate point.coordinate.x (point.coordinate.y + 1)
                            )

                        DOWN ->
                            ( Coordinate point.coordinate.x (point.coordinate.y - 2)
                            , Coordinate point.coordinate.x (point.coordinate.y - 1)
                            )

                        RIGHT ->
                            ( Coordinate (point.coordinate.x + 2) point.coordinate.y
                            , Coordinate (point.coordinate.x + 1) point.coordinate.y
                            )

                        LEFT ->
                            ( Coordinate (point.coordinate.x - 2) point.coordinate.y
                            , Coordinate (point.coordinate.x - 1) point.coordinate.y
                            )
            in
            case diggingSpecifiedDirection point.coordinate c1 c2 room of
                Just roomPoints ->
                    let
                        nextCandidates =
                            Maybe.withDefault point (ListE.find (\p -> p.coordinate == c2) room.points)
                                |> (\p -> p :: room.candidates)
                    in
                    generateRoom
                        { room
                            | points = roomPoints
                            , candidates = nextCandidates
                            , directions = newDirections
                            , startCoordinate = point.coordinate
                        }

                Nothing ->
                    let
                        newPoint =
                            { point | possibleDirections = ListE.remove direction point.possibleDirections }
                    in
                    if List.isEmpty newPoint.possibleDirections then
                        generateRoom { room | candidates = points, directions = newDirections }

                    else
                        generateRoom { room | candidates = newPoint :: points, directions = newDirections }


diggingSpecifiedDirection : Coordinate -> Coordinate -> Coordinate -> Room -> Maybe RoomPoints
diggingSpecifiedDirection c0 c1 c2 room =
    if List.any (\p -> c2 == p.coordinate && p.coordinateStatus == Wall) room.points then
        Just (ListE.updateIf (\p -> c0 == p.coordinate || c2 == p.coordinate || c1 == p.coordinate && p.coordinateStatus == Wall) (\p -> { p | coordinateStatus = Road }) room.points)

    else
        Nothing


canTheWallBreak : Room -> Point -> Bool
canTheWallBreak room point =
    point.coordinateStatus
        == Wall
        && not (isPerimeter room.maxOfCoordinate point.coordinate.x point.coordinate.y)
        && (remainderBy 3 point.coordinate.x == 0 || remainderBy 3 point.coordinate.x == 0)


breakThroughTheWall : Room -> Room
breakThroughTheWall room =
    { room
        | points =
            ListE.updateIf (\p -> canTheWallBreak room p)
                (\p -> { p | coordinateStatus = Road })
                room.points
    }
