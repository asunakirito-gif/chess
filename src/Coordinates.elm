module Coordinates exposing (moveKnight)

import Board exposing (Board)
import Cell exposing (Cell)
import Piece exposing (Color(..), Kind(..), Piece)


moveKnight : Board -> Board
moveKnight board =
    board
        |> moveKnight 1 7 { kind = Pawn, color = Black }



--        |> moveKnight 2 1 { kind = Knight }
--        |> moveKnight 2 -1 { kind = Knight }
--        |> moveKnight -2 1 { kind = Knight }
--        |> moveKnight -2 -1 { kind = Knight }
--        |> moveKnight 1 2 { kind = Knight }
--        |> moveKnight -1 2 { kind = Knight }
--        |> moveKnight 1 -2 { kind = Knight }
--        |> moveKnight -1 -2 { kind = Knight }
