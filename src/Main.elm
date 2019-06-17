module Main exposing (Model, main)

import Array2D
import Board exposing (Board)
import Browser exposing (element)
import Cell exposing (Cell)
import Html.Styled exposing (Html, div, toUnstyled)
import Piece exposing (Piece)


type alias Model =
    { board : Board
    , state : State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        _ =
            Debug.log "msg" (Board.initialBoard |> Board.placePieces)
    in
    ( { board = Board.initialBoard |> Board.placePieces, state = Initial }, Cmd.none )


type State
    = Initial
    | Selected Cell


type Msg
    = BoardMsg Board.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    case msg of
        BoardMsg message ->
            case message of
                Board.Click cell ->
                    case ( cell.piece, model.state ) of
                        ( Just piece, Initial ) ->
                            ( { model
                                | state = Selected cell
                                , board = Array2D.set cell.row cell.column { cell | isSelected = True } model.board
                              }
                            , Cmd.none
                            )

                        ( Nothing, Initial ) ->
                            ( model, Cmd.none )

                        ( Nothing, Selected c ) ->
                            ( model, Cmd.none )

                        ( Just piece, Selected c ) ->
                            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Board.render model.board |> Html.Styled.map BoardMsg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
