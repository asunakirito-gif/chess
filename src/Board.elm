module Board exposing (Board, initial, movePiece)

import Array2D exposing (Array2D)
import Cell exposing (Cell)
import Piece exposing (Color(..), Kind(..), Piece)


type alias Board =
    Array2D Cell


initial : Board
initial =
    Array2D.repeat 8 8 { piece = Nothing, row = 0, column = 0 }
        |> Array2D.indexedMap setCoordinate
        |> placePieces


setCoordinate : Int -> Int -> Cell -> Cell
setCoordinate row column cell =
    { cell | row = row, column = column }


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
    case Array2D.get row column board of
        Just cell ->
            Array2D.set row column { cell | piece = Just piece } board

        Nothing ->
            board


removePiece : Cell -> Board -> Board
removePiece cell board =
    Array2D.set cell.row cell.column { cell | piece = Nothing } board


addPiece : Cell -> Piece -> Board -> Board
addPiece cell piece board =
    Array2D.set cell.row cell.column { cell | piece = Just piece } board


movePiece : Cell -> Cell -> Board -> Board
movePiece from to board =
    case from.piece of
        Just piece ->
            board
                |> removePiece from
                |> addPiece to piece

        Nothing ->
            board
