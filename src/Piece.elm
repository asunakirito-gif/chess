module Piece exposing (Color(..), Kind(..), Piece, render)


type Color
    = Black
    | White


type Kind
    = King
    | Queen
    | Rook
    | Bishop
    | Knight
    | Pawn


type alias Piece =
    { kind : Kind
    , color : Color
    }


render : Piece -> Char
render { kind, color } =
    case ( kind, color ) of
        ( King, Black ) ->
            '♚'

        ( Queen, Black ) ->
            '♛'

        ( Rook, Black ) ->
            '♜'

        ( Bishop, Black ) ->
            '♝'

        ( Knight, Black ) ->
            '♞'

        ( Pawn, Black ) ->
            '♟'

        ( King, White ) ->
            '♔'

        ( Queen, White ) ->
            '♕'

        ( Rook, White ) ->
            '♖'

        ( Bishop, White ) ->
            '♗'

        ( Knight, White ) ->
            '♘'

        ( Pawn, White ) ->
            '♙'
