module Board exposing
    ( Board
    , Cell
    , Stone(..)
    , countStone
    , getPuttableState
    , initBoard
    , putStone
    , reverse
    , setStone
    )

import List.Extra as List
import Maybe.Extra as Maybe
import Position as Pos exposing (Position)


type alias Board =
    List (List Cell)


type Stone
    = White
    | Black


type alias Cell =
    { stone : Maybe Stone
    , position : Position
    }


initBoard : List (List Cell)
initBoard =
    List.map
        (\y ->
            List.map
                (\x ->
                    case ( x, y ) of
                        ( 3, 3 ) ->
                            { stone = Just White, position = { x = x, y = y } }

                        ( 3, 4 ) ->
                            { stone = Just Black, position = { x = x, y = y } }

                        ( 4, 3 ) ->
                            { stone = Just Black, position = { x = x, y = y } }

                        ( 4, 4 ) ->
                            { stone = Just White, position = { x = x, y = y } }

                        _ ->
                            { stone = Nothing, position = { x = x, y = y } }
                )
            <|
                List.range 0 7
        )
    <|
        List.range 0 7


fold : (Cell -> a -> a) -> a -> Board -> a
fold operation acc board =
    List.foldl
        (\row acc_ ->
            List.foldl (\cell acc__ -> operation cell acc__) acc_ row
        )
        acc
        board


getPuttableState : Stone -> Board -> { puttable : List Position, unputtable : List Position }
getPuttableState stone board =
    fold
        (\cell acc ->
            case cell.stone of
                Nothing ->
                    case searchReversibleStones cell.position stone board of
                        [] ->
                            { acc | unputtable = cell.position :: acc.unputtable }

                        ps ->
                            { acc | puttable = cell.position :: acc.puttable }

                Just _ ->
                    { acc | unputtable = cell.position :: acc.unputtable }
        )
        { puttable = [], unputtable = [] }
        board


countStone : Board -> { black : Int, white : Int }
countStone =
    fold
        (\cell acc ->
            case cell.stone of
                Just Black ->
                    { acc | black = acc.black + 1 }

                Just White ->
                    { acc | white = acc.white + 1 }

                Nothing ->
                    acc
        )
        { black = 0, white = 0 }


putStone :
    Position
    -> Stone
    -> Board
    -> Result String Board
putStone pos turn src =
    case getStone pos src of
        Just _ ->
            Err "すでに石が置かれています。"

        Nothing ->
            case searchReversibleStones pos turn src of
                [] ->
                    Err "ひっくり返せる石がありません。"

                ps ->
                    List.foldr (reverseStone turn) src ps
                        |> setStone pos turn


getStone : Position -> Board -> Maybe Stone
getStone { x, y } board =
    List.getAt y board
        |> Maybe.map (List.getAt x)
        |> Maybe.join
        |> Maybe.map .stone
        |> Maybe.join


searchReversibleStones : Position -> Stone -> Board -> List Position
searchReversibleStones startPos stone board =
    (toReversiblePositions <| searchReversibleStonesCore Pos.up stone board 0 startPos [])
        ++ (toReversiblePositions <| searchReversibleStonesCore Pos.down stone board 0 startPos [])
        ++ (toReversiblePositions <| searchReversibleStonesCore Pos.right stone board 0 startPos [])
        ++ (toReversiblePositions <| searchReversibleStonesCore Pos.left stone board 0 startPos [])
        ++ (toReversiblePositions <| searchReversibleStonesCore Pos.upRight stone board 0 startPos [])
        ++ (toReversiblePositions <| searchReversibleStonesCore Pos.upLeft stone board 0 startPos [])
        ++ (toReversiblePositions <| searchReversibleStonesCore Pos.downRight stone board 0 startPos [])
        ++ (toReversiblePositions <| searchReversibleStonesCore Pos.downLeft stone board 0 startPos [])


toReversiblePositions : ( SearchResult, List Position ) -> List Position
toReversiblePositions ( result, ps ) =
    case result of
        NeighborIsSameColor ->
            ps

        _ ->
            []


type SearchResult
    = Edge
    | NoNeighbor
    | NeighborIsSameColor
    | NeighborIsDifferentColor


searchReversibleStonesCore :
    (Position -> Maybe Position)
    -> Stone
    -> Board
    -> Int
    -> Position
    -> List Position
    -> ( SearchResult, List Position )
searchReversibleStonesCore moveOperation putStoneColor board recursionCount startPos acc =
    case moveOperation startPos of
        Nothing ->
            ( Edge, acc )

        Just pos ->
            case getStone pos board of
                Nothing ->
                    ( NoNeighbor, acc )

                Just stone ->
                    if stone == putStoneColor then
                        -- 挟む石が見つかった場合がここ. 探索終了
                        ( NeighborIsSameColor, acc )

                    else
                        searchReversibleStonesCore
                            moveOperation
                            putStoneColor
                            board
                            (recursionCount + 1)
                            pos
                            (pos :: acc)


setStone : Position -> Stone -> Board -> Result String Board
setStone { x, y } stone board =
    case List.getAt y board of
        Nothing ->
            Err "範囲外です。"

        Just row ->
            case List.getAt x row of
                Nothing ->
                    Err "範囲外です。"

                Just cell ->
                    case cell.stone of
                        Just _ ->
                            Err "すでに石が置かれています。"

                        Nothing ->
                            Ok
                                (List.setAt y
                                    (List.setAt x
                                        { cell | stone = Just stone }
                                        row
                                    )
                                    board
                                )


reverseStone : Stone -> Position -> Board -> Board
reverseStone stone { x, y } board =
    case List.getAt y board of
        Nothing ->
            board

        Just row ->
            case List.getAt x row of
                Nothing ->
                    board

                Just cell ->
                    case cell.stone of
                        Just now ->
                            List.setAt y
                                (List.setAt x
                                    { cell | stone = Just <| reverse now }
                                    row
                                )
                                board

                        Nothing ->
                            board


reverse : Stone -> Stone
reverse stone =
    case stone of
        White ->
            Black

        Black ->
            White

