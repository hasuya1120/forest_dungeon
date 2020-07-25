module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Board exposing (Board, Directions, MaybeCandidates, chooseDirectionsAndCandidate, updateRoom, viewRoom)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board }


init : ( Model, Cmd Msg )
init =
    let
        board =
            Board.init 30
    in
    ( { board = board }
    , Random.generate ChooseDirectionsCandidateForPrimary (chooseDirectionsAndCandidate board.primaryRoom.candidates 30)
    )



-- UPDATE


type Msg
    = ChooseDirectionsCandidateForPrimary ( MaybeCandidates, Directions )
    | ChooseDirectionsCandidateForSecondary ( MaybeCandidates, Directions )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseDirectionsCandidateForPrimary ( maybeCandidate, directions ) ->
            let
                currentBoard =
                    model.board

                newPrimaryRoom =
                    updateRoom maybeCandidate directions currentBoard.primaryRoom

                newBoard =
                    { currentBoard | primaryRoom = newPrimaryRoom }
            in
            ( { model | board = newBoard }
            , Random.generate
                ChooseDirectionsCandidateForSecondary
                (chooseDirectionsAndCandidate newBoard.secondaryRoom.candidates 30)
            )

        ChooseDirectionsCandidateForSecondary ( maybeCandidate, directions ) ->
            let
                currentBoard =
                    model.board

                newSecondaryRoom =
                    updateRoom maybeCandidate directions currentBoard.secondaryRoom

                newBoard =
                    { currentBoard | secondaryRoom = newSecondaryRoom }
            in
            ( { model | board = newBoard }, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "game_board" ]
        [ div [ class "container" ] (viewRoom model.board.primaryRoom) ]
