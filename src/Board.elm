module Board exposing (Board, initialBoard, render, test)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Cell exposing (Cell)
import Css exposing (border2, displayFlex, height, px, solid, width)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)


type alias Board =
    Array2D Cell


size : Float
size =
    20


test =
    Array.get 0 initialBoard.data


initialBoard : Board
initialBoard =
    Array2D.repeat 8 8 { piece = Nothing }


render : Board -> Html msg
render board =
    div [] (Array.map renderRow board.data |> Array.toList)


renderRow : Array Cell -> Html msg
renderRow row =
    div [ css [ displayFlex ] ] (Array.map renderCell row |> Array.toList)


renderCell : Cell -> Html msg
renderCell cell =
    div
        [ css
            [ width (px size)
            , height (px size)
            , border2 (px 1) solid
            ]
        ]
        []
