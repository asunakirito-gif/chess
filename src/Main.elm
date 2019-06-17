module Main exposing (Model, main)

import Array2D
import Board exposing (Board)
import Browser exposing (element)
import Html.Styled exposing (Html, div, toUnstyled)


type alias Model =
    { board : Board }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.initialBoard }, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Board.render model.board


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
