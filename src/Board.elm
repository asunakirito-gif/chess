module Board exposing
    ( Board
    , initial
    , movePiece
    , possibleBishopMovements
    , possibleKingMovements
    , possibleKnightMovements
    , possibleQueenMovements
    , possibleRookMovements
    )

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


removePiece : ( Int, Int ) -> Board -> Board
removePiece ( row, column ) board =
    case Array2D.get row column board of
        Just cell ->
            Array2D.set row column { cell | piece = Nothing } board

        Nothing ->
            board


addPiece : Piece -> ( Int, Int ) -> Board -> Board
addPiece piece ( row, column ) board =
    case Array2D.get row column board of
        Just cell ->
            Array2D.set cell.row cell.column { cell | piece = Just piece } board

        Nothing ->
            board


movePiece : Piece -> ( Int, Int ) -> ( Int, Int ) -> Board -> Board
movePiece piece from to board =
    board
        |> removePiece from
        |> addPiece piece to


computeKnightMovements : ( Int, Int ) -> List ( Int, Int )
computeKnightMovements ( row, column ) =
    [ ( row + 1, column + 2 )
    , ( row + 1, column - 2 )
    , ( row - 1, column + 2 )
    , ( row - 1, column - 2 )
    , ( row + 2, column + 1 )
    , ( row + 2, column - 1 )
    , ( row - 2, column + 1 )
    , ( row - 2, column - 1 )
    ]


computeKingMovements : ( Int, Int ) -> List ( Int, Int )
computeKingMovements ( row, column ) =
    [ ( row + 1, column + 1 )
    , ( row + 1, column - 1 )
    , ( row + 1, column + 0 )
    , ( row - 1, column - 1 )
    , ( row - 1, column + 1 )
    , ( row - 1, column + 0 )
    , ( row + 0, column + 1 )
    , ( row + 0, column - 1 )
    ]


type alias Move =
    ( Int, Int ) -> ( Int, Int )


topLeftMovement : Move
topLeftMovement ( row, column ) =
    ( row - 1, column - 1 )


topRightMovement : Move
topRightMovement ( row, column ) =
    ( row - 1, column + 1 )


downLeftMovement : Move
downLeftMovement ( row, column ) =
    ( row + 1, column - 1 )


downRightMovement : Move
downRightMovement ( row, column ) =
    ( row + 1, column + 1 )


topMovement : Move
topMovement ( row, column ) =
    ( row - 1, column )


downMovement : Move
downMovement ( row, column ) =
    ( row + 1, column )


rightMovement : Move
rightMovement ( row, column ) =
    ( row, column + 1 )


leftMovement : Move
leftMovement ( row, column ) =
    ( row, column - 1 )


verifyMovement :
    Board
    -> ( Int, Int )
    -> Color
    -> List ( Int, Int )
    -> Move
    -> List ( Int, Int )
verifyMovement board ( x, y ) color list move =
    let
        ( xp, yp ) =
            move ( x, y )
    in
    case Array2D.get xp yp board of
        Nothing ->
            list

        Just cell ->
            case cell.piece of
                Nothing ->
                    verifyMovement board ( xp, yp ) color (( xp, yp ) :: list) move

                Just piece ->
                    if piece.color == color then
                        list

                    else
                        ( xp, yp ) :: list


rookMoves =
    [ topMovement, downMovement, leftMovement, rightMovement ]


bishopMoves =
    [ topRightMovement, downRightMovement, topLeftMovement, downRightMovement ]


queenMoves =
    rookMoves ++ bishopMoves


possibleRookMovements : Board -> ( Int, Int ) -> Color -> List ( Int, Int )
possibleRookMovements board coord color =
    possibleMovementsWithDirections board coord color rookMoves


possibleBishopMovements : Board -> ( Int, Int ) -> Color -> List ( Int, Int )
possibleBishopMovements board coord color =
    possibleMovementsWithDirections board coord color bishopMoves


possibleQueenMovements : Board -> ( Int, Int ) -> Color -> List ( Int, Int )
possibleQueenMovements board coord color =
    possibleMovementsWithDirections board coord color queenMoves


possibleMovementsWithDirections : Board -> ( Int, Int ) -> Color -> List Move -> List ( Int, Int )
possibleMovementsWithDirections board coord color moves =
    moves
        |> List.map (verifyMovement board coord color [])
        |> List.concat


get : Board -> ( Int, Int ) -> Maybe Cell
get board ( x, y ) =
    Array2D.get x y board


keep : Color -> Cell -> Bool
keep color cell =
    case cell.piece of
        Nothing ->
            True

        Just piece ->
            if piece.color == color then
                False

            else
                True


possibleMovements : (( Int, Int ) -> List ( Int, Int )) -> ( Int, Int ) -> Board -> Color -> List ( Int, Int )
possibleMovements compute coord board color =
    coord
        |> compute
        |> List.map (get board)
        |> List.filterMap identity
        |> List.filter (keep color)
        |> List.map (\cell -> ( cell.row, cell.column ))


possibleKingMovements : ( Int, Int ) -> Board -> Color -> List ( Int, Int )
possibleKingMovements =
    possibleMovements computeKingMovements


possibleKnightMovements : ( Int, Int ) -> Board -> Color -> List ( Int, Int )
possibleKnightMovements =
    possibleMovements computeKnightMovements


foo : Piece -> ( Int, Int ) -> Board -> List ( Int, Int )
foo piece coord board =
    case piece.kind of
        King ->
            possibleKingMovements coord board piece.color

        Queen ->
            possibleQueenMovements board coord piece.color

        Bishop ->
            possibleBishopMovements board coord piece.color

        Knight ->
            possibleKnightMovements coord board piece.color

        Rook ->
            possibleRookMovements board coord piece.color

        Pawn ->
            []
