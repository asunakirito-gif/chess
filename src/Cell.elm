module Cell exposing (Cell)

import Piece exposing (Piece)


type alias Cell =
    { piece : Maybe Piece }
