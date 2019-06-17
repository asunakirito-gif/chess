module Board exposing (Board, Msg(..), initialBoard, placePieces, render, test)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Cell exposing (Cell)
import Css exposing (backgroundColor, border2, displayFlex, fontSize, height, px, rgb, solid, width)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Piece exposing (Color(..), Kind(..), Piece)


type alias Board =
    Array2D Cell


size : Float
size =
    80


test =
    Array.get 0 initialBoard.data


setCoordinate : Int -> Int -> Cell -> Cell
setCoordinate row column cell =
    { cell | row = row, column = column }


initialBoard : Board
initialBoard =
    Array2D.repeat 8 8 { piece = Nothing, row = 0, column = 0, isSelected = False }
        |> Array2D.indexedMap setCoordinate


placePieces : Board -> Board
placePieces board =
    board
        |> placePiece 7 0 { kind = Rook, color = White }
        |> placePiece 7 1 { kind = Knight, color = White }
        |> placePiece 7 2 { kind = Bishop, color = White }
        |> placePiece 7 3 { kind = Queen, color = White }
        |> placePiece 7 4 { kind = King, color = White }
        |> placePiece 7 5 { kind = Bishop, color = White }
        |> placePiece 7 6 { kind = Knight, color = White }
        |> placePiece 7 7 { kind = Rook, color = White }
        |> placePiece 6 0 { kind = Pawn, color = White }
        |> placePiece 6 1 { kind = Pawn, color = White }
        |> placePiece 6 2 { kind = Pawn, color = White }
        |> placePiece 6 3 { kind = Pawn, color = White }
        |> placePiece 6 4 { kind = Pawn, color = White }
        |> placePiece 6 5 { kind = Pawn, color = White }
        |> placePiece 6 6 { kind = Pawn, color = White }
        |> placePiece 6 7 { kind = Pawn, color = White }
        |> placePiece 0 0 { kind = Rook, color = Black }
        |> placePiece 0 1 { kind = Knight, color = Black }
        |> placePiece 0 2 { kind = Bishop, color = Black }
        |> placePiece 0 3 { kind = Queen, color = Black }
        |> placePiece 0 4 { kind = King, color = Black }
        |> placePiece 0 5 { kind = Bishop, color = Black }
        |> placePiece 0 6 { kind = Knight, color = Black }
        |> placePiece 0 7 { kind = Rook, color = Black }
        |> placePiece 1 0 { kind = Pawn, color = Black }
        |> placePiece 1 1 { kind = Pawn, color = Black }
        |> placePiece 1 2 { kind = Pawn, color = Black }
        |> placePiece 1 3 { kind = Pawn, color = Black }
        |> placePiece 1 4 { kind = Pawn, color = Black }
        |> placePiece 1 5 { kind = Pawn, color = Black }
        |> placePiece 1 6 { kind = Pawn, color = Black }
        |> placePiece 1 7 { kind = Pawn, color = Black }


placePiece : Int -> Int -> Piece -> Board -> Board
placePiece row column piece board =
    Array2D.set row column { piece = Just piece, row = row, column = column, isSelected = False } board


render : Board -> Html Msg
render board =
    div [] (Array.map renderRow board.data |> Array.toList)


renderRow : Array Cell -> Html Msg
renderRow row =
    div [ css [ displayFlex ] ] (Array.map renderCell row |> Array.toList)


type Msg
    = Click Cell


renderCell : Cell -> Html Msg
renderCell cell =
    let
        label =
            case cell.piece of
                Just piece ->
                    Piece.render piece |> String.fromChar

                Nothing ->
                    ""
    in
    div
        [ css
            [ width (px size)
            , height (px size)
            , border2 (px 1) solid
            , fontSize (px 65)
            , backgroundColor
                (if cell.isSelected then
                    rgb 255 0 0

                 else
                    rgb 255 255 255
                )
            ]
        , onClick (Click cell)
        ]
        [ text label ]
