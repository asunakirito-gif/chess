module Main exposing (Model, main)

import Array exposing (Array)
import Board exposing (Board)
import Browser exposing (element)
import Cell exposing (Cell)
import Css
    exposing
        ( backgroundColor
        , border2
        , displayFlex
        , fontSize
        , height
        , px
        , rgb
        , solid
        , width
        )
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Piece


type alias Model =
    { board : Board
    , state : State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        _ =
            Debug.log "msg" Board.initial
    in
    ( { board = Board.initial, state = Initial }, Cmd.none )


type State
    = Initial
    | Selected Cell


type Msg
    = Click Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    case msg of
        Click cell ->
            case ( model.state, cell.piece ) of
                ( Initial, Just piece ) ->
                    ( { model | state = Selected cell }, Cmd.none )

                ( Initial, Nothing ) ->
                    ( model, Cmd.none )

                ( Selected c, Nothing ) ->
                    ( model, Cmd.none )

                ( Selected c, Just piece ) ->
                    ( model, Cmd.none )



-- view


size : Float
size =
    80


view : Model -> Html Msg
view model =
    render model.state model.board


render : State -> Board -> Html Msg
render state board =
    div [] (Array.map (renderRow state) board.data |> Array.toList)


renderRow : State -> Array Cell -> Html Msg
renderRow state row =
    div [ css [ displayFlex ] ] (Array.map (renderCell state) row |> Array.toList)


renderCell : State -> Cell -> Html Msg
renderCell state cell =
    let
        label =
            case cell.piece of
                Just piece ->
                    Piece.render piece |> String.fromChar

                Nothing ->
                    ""

        color =
            case state of
                Initial ->
                    rgb 255 255 255

                Selected selectedCell ->
                    if cell == selectedCell then
                        rgb 255 0 0

                    else
                        rgb 255 255 255
    in
    div
        [ css
            [ width (px size)
            , height (px size)
            , border2 (px 1) solid
            , fontSize (px 65)
            , backgroundColor color
            ]
        , onClick (Click cell)
        ]
        [ text label ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
