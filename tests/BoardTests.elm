module Main exposing (buildBoard, emptyBoard, suite)

import Board exposing (..)
import Expect exposing (equal)
import Position exposing (Position)
import Test exposing (..)


suite : Test
suite =
    describe "Board.putStone"
        [ describe "黒でひっくり返す"
            [ test "(3,2)に黒を置く"
                (\_ ->
                    let
                        actualBoard =
                            putStone { x = 3, y = 2 } Black initBoard

                        expectedBoard =
                            Ok <|
                                buildBoard
                                    { blackPositions =
                                        [ { x = 3, y = 2 }
                                        , { x = 3, y = 3 }
                                        , { x = 3, y = 4 }
                                        , { x = 4, y = 3 }
                                        ]
                                    , whitePositions = [ { x = 4, y = 4 } ]
                                    }
                    in
                    Expect.equal actualBoard expectedBoard
                )
            ]
        , describe "初期状態でエッジに石を置く"
            [ test "左上に黒を置く"
                (\_ ->
                    let
                        initialBoard =
                            initBoard

                        expectedBoard =
                            Err "ひっくり返せる石がありません。"

                        actualBoard =
                            putStone { x = 0, y = 0 } Black initialBoard
                    in
                    Expect.equal actualBoard expectedBoard
                )
            , test "右上に黒を置く"
                (\_ ->
                    let
                        initialBoard =
                            initBoard

                        expectedBoard =
                            Err "ひっくり返せる石がありません。"

                        actualBoard =
                            putStone { x = 7, y = 0 } Black initialBoard
                    in
                    Expect.equal actualBoard expectedBoard
                )
            , test "左下に黒を置く"
                (\_ ->
                    let
                        initialBoard =
                            initBoard

                        expectedBoard =
                            Err "ひっくり返せる石がありません。"

                        actualBoard =
                            putStone { x = 0, y = 7 } Black initialBoard
                    in
                    Expect.equal actualBoard expectedBoard
                )
            , test "右下に黒を置く"
                (\_ ->
                    let
                        initialBoard =
                            initBoard

                        expectedBoard =
                            Err "ひっくり返せる石がありません。"

                        actualBoard =
                            putStone { x = 7, y = 7 } Black initialBoard
                    in
                    Expect.equal actualBoard expectedBoard
                )
            ]
        ]


buildBoard :
    { blackPositions : List Position
    , whitePositions : List Position
    }
    -> Board
buildBoard { blackPositions, whitePositions } =
    emptyBoard
        |> (\board ->
                List.foldr
                    (\blackPos acc ->
                        setStone blackPos Black acc |> Result.withDefault emptyBoard
                    )
                    board
                    blackPositions
           )
        |> (\board ->
                List.foldr
                    (\whiltePos acc ->
                        setStone whiltePos White acc |> Result.withDefault emptyBoard
                    )
                    board
                    whitePositions
           )


emptyBoard : Board
emptyBoard =
    List.map
        (\y ->
            List.map
                (\x ->
                    { stone = Nothing, position = { x = x, y = y } }
                )
            <|
                List.range 0 7
        )
    <|
        List.range 0 7
