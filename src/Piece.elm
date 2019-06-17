module Piece exposing (Piece)


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
