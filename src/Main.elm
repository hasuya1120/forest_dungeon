module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Random
import Room exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { room : Room }


init : ( Model, Cmd Msg )
init =
    let
        room =
            initializeRoom 30
    in
    ( { room = room }
    , Random.generate ChooseCandidate (chooseCandidate room.candidates)
    )



-- UPDATE


type Msg
    = ChooseCandidate ( Maybe Point, List Point )
    | ChooseDirections (List Direction)
    | GeneratingRoom


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseCandidate ( maybeCandidate, _ ) ->
            let
                currentRoom =
                    model.room

                newRoom =
                    { currentRoom | candidates = Maybe.withDefault [] (Maybe.map List.singleton maybeCandidate) }
            in
            ( { model | room = newRoom }
            , Random.generate ChooseDirections (chooseDirections newRoom.maxOfCoordinate)
            )

        ChooseDirections directions ->
            let
                currentRoom =
                    model.room

                newRoom =
                    { currentRoom | directions = directions }
            in
            update GeneratingRoom { model | room = newRoom }

        GeneratingRoom ->
            ( { model | room = generateRoom model.room }, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "game_board" ]
        [ div [ class "container" ]
            (List.map
                (\p -> viewPointWithin1PointWithCharacter p)
                model.room.points
            )
        ]


viewPointWithin1PointWithCharacter : Point -> Html Msg
viewPointWithin1PointWithCharacter point =
    case point.coordinateStatus of
        Wall ->
            div [ class "wall" ] [ text "🌲" ]

        Road ->
            div [ class "road" ] [ text "☘️" ]
