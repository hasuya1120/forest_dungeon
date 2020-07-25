module Board exposing (Board, Directions, MaybeCandidates, chooseDirectionsAndCandidate, init, roomHasStairs, updateRoom, viewRoom)

import Html exposing (..)
import Html.Attributes exposing (..)
import Random exposing (Generator)
import Random.List
import Room exposing (Candidates, Coordinate, CoordinateStatus(..), Direction, Point, Room, generateRoom)


type alias Board =
    { primaryRoom : Room
    , secondaryRoom : Room
    , numberOfTrips : Int
    }


init : Int -> Board
init maxOfCoordinate =
    { primaryRoom = Room.initializeRoom maxOfCoordinate
    , secondaryRoom = Room.initializeRoom maxOfCoordinate
    , numberOfTrips = 0
    }


updateRoom : MaybeCandidates -> Directions -> Room -> Room
updateRoom ( maybeCandidate, _ ) directions room =
    generateRoom
        { room
            | candidates = Maybe.withDefault [] (Maybe.map List.singleton maybeCandidate)
            , directions = directions
            , stairsCoordinate = Maybe.withDefault (Coordinate 0 0) (Maybe.map .coordinate maybeCandidate)
        }



-- Choose Candidate and Directions generator


type alias MaybeCandidates =
    ( Maybe Point, List Point )


type alias Directions =
    List Direction


chooseCandidate : Candidates -> Generator MaybeCandidates
chooseCandidate candidates =
    Random.List.choose candidates


chooseDirection : Generator Direction
chooseDirection =
    Random.uniform Room.UP [ Room.DOWN, Room.RIGHT, Room.LEFT ]


chooseDirections : Int -> Generator (List Direction)
chooseDirections maxOfCoordinate =
    Random.list ((maxOfCoordinate ^ 2) * 2) chooseDirection


roomHasStairs : Generator Bool
roomHasStairs =
    Random.uniform True [ False ]


chooseDirectionsAndCandidate : Candidates -> Int -> Generator ( MaybeCandidates, Directions )
chooseDirectionsAndCandidate candidates maxOfCoordinate =
    Random.pair (chooseCandidate candidates) (chooseDirections maxOfCoordinate)



-- view


viewRoom : Room -> List (Html msg)
viewRoom room =
    List.map
        (\p -> viewPoint p room)
        room.points


viewPoint : Point -> Room -> Html msg
viewPoint point room =
    if point.coordinate == room.startCoordinate then
        div [ class "start_point" ] [ text "🙍\u{200D}♂️" ]

    else if point.coordinate == room.stairsCoordinate then
        div [ class "goal_point" ] [ text "🕳️" ]

    else
        case point.coordinateStatus of
            Wall ->
                div [ class "wall" ] [ text "🌲" ]

            Road ->
                div [ class "road" ] [ text "☘️" ]
