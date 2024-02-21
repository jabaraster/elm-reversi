port module Reversi exposing (..)

import Board exposing (..)
import Board.Derive exposing (decodeCell, decodeStone, encodeBoard, encodeStone)
import Browser
import Css exposing (..)
import Css.Animations as Animations exposing (keyframes)
import Css.Transitions as Transitions
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Html.Styled.Events exposing (..)
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy as Lazy
import Json.Decode exposing (Value)
import Json.Encode
import Position as Pos exposing (Position)
import Position.Derive exposing (decodePosition, encodePosition)
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( initModel
                , Cmd.none
                )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveSavedModel GotSavedModel


type alias Model =
    { board : Board
    , turn : Stone
    , message : String
    }


initModel : Model
initModel =
    { board = initBoard
    , turn = Black
    , message = "ゲーム開始！"
    }


encodeModel : Model -> Json.Encode.Value
encodeModel =
    \value0 ->
        Json.Encode.object
            [ ( "board", encodeBoard value0.board )
            , ( "turn", encodeStone value0.turn )
            , ( "message", Json.Encode.string value0.message )
            ]


decodeModel : Json.Decode.Decoder Model
decodeModel =
    Json.Decode.succeed Model
        |> decodeAndMap (Json.Decode.field "board" decodeBoard)
        |> decodeAndMap (Json.Decode.field "turn" decodeStone)
        |> decodeAndMap (Json.Decode.field "message" Json.Decode.string)


decodeBoard : Json.Decode.Decoder Board
decodeBoard =
    Json.Decode.list (Json.Decode.list decodeCell)


decodeAndMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
decodeAndMap =
    Json.Decode.map2 (|>)


type Msg
    = OnPutStone Position
    | OnPass
    | OnAgain
    | OnSave
    | OnRestore
    | GotSavedModel String


port saveModel : String -> Cmd msg


port requestGetSavedModel : () -> Cmd msg


port receiveSavedModel : (String -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPutStone pos ->
            case putStone pos model.turn model.board of
                Ok board ->
                    ( { model
                        | board = board
                        , turn = reverse model.turn
                        , message = ""
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | message = err }, Cmd.none )

        OnPass ->
            ( { model | turn = reverse model.turn, message = "パスしました" }, Cmd.none )

        OnAgain ->
            ( initModel, Cmd.none )

        OnSave ->
            ( { model | message = "保存しました" }, saveModel <| Json.Encode.encode 0 <| encodeModel model )

        OnRestore ->
            ( model, requestGetSavedModel () )

        GotSavedModel value ->
            let
                m =
                    Json.Decode.decodeString decodeModel value
                        |> Result.withDefault model
            in
            ( { m | message = "復元しました" }, Cmd.none )


type TurnState
    = Puttable
    | Pass
    | GameOver


getTurnState : Model -> TurnState
getTurnState model =
    let
        puttableState =
            getPuttableState model.turn model.board
    in
    if List.isEmpty puttableState.puttable then
        if (getPuttableState (reverse model.turn) model.board).puttable |> List.isEmpty then
            GameOver

        else
            Pass

    else
        Puttable


view : Model -> Html.Html Msg
view model =
    let
        cnt =
            countStone model.board
    in
    Html.Styled.toUnstyled <|
        let
            puttableState =
                getPuttableState model.turn model.board
        in
        div [] <|
            (List.map (viewBoardRow <| Set.fromList <| List.map Pos.toKey puttableState.unputtable) model.board
                ++ [ hr [] [] ]
                ++ (case getTurnState model of
                        GameOver ->
                            [ viewGameOver ]

                        Pass ->
                            [ viewPass ]

                        Puttable ->
                            []
                   )
                ++ [ div [ css [ fontSize (pct 150) ] ]
                        [ div [ css styleScore ] [ viewStone (Just Black) True, text ":", text <| String.fromInt cnt.black ]
                        , div [ css styleScore ] [ viewStone (Just White) True, text ":", text <| String.fromInt cnt.white ]
                        ]
                   , div [] [ text "手番 : ", viewStone (Just model.turn) True ]
                   , div []
                        [ p []
                            [ text <|
                                if model.message == "" then
                                    "\u{3000}"

                                else
                                    model.message
                            ]
                        ]
                   , hr [] []
                   , p [] [ text "ルール参照: ", linkToRule ]
                   , hr [] []
                   , button [ onClick OnSave ] [ text "保存" ]
                   , button [ onClick OnRestore ] [ text "復元" ]
                   ]
            )


viewGameOver : Html Msg
viewGameOver =
    viewInformation (rgba 100 230 100 0.3)
        [ p [ css [ fontSize (pct 120) ] ] [ text "ゲーム終了！" ]
        , button
            [ css [ padding (px 4), fontSize (pct 120) ]
            , onClick OnAgain
            ]
            [ text "もう１度遊ぶ" ]
        ]


viewPass : Html Msg
viewPass =
    viewInformation (rgba 255 255 255 0.3)
        [ p [] [ text "置ける場所がありません。" ]
        , button
            [ css [ fontSize (pct 120) ]
            , onClick OnPass
            ]
            [ text "パス" ]
        ]


viewInformation : Color -> List (Html Msg) -> Html Msg
viewInformation bgColor children =
    div
        [ css
            [ paddingTop (px 4)
            , paddingRight (px 8)
            , paddingLeft (px 8)
            , paddingBottom (px 8)
            , borderRadius (px 4)
            , margin (px 2)
            , backgroundColor bgColor
            , animationName <|
                keyframes
                    [ ( 0
                      , [ Animations.opacity (int 0)
                        , Animations.transform [ translateY (pct -100) ]
                        ]
                      )
                    , ( 50
                      , [ Animations.opacity (int 0)
                        , Animations.transform [ translateY (pct -50) ]
                        ]
                      )
                    , ( 100, [ Animations.opacity (int 100) ] )
                    ]
            , animationDuration (ms 300)
            ]
        ]
        children


viewBoardRow : Set String -> List Cell -> Html Msg
viewBoardRow cannotPutPositions row =
    Keyed.node "div" [] <| List.map (viewCell cannotPutPositions) row


viewCell : Set String -> Cell -> ( String, Html Msg )
viewCell cannotPutPositions cell =
    ( Pos.toKey cell.position
    , div
        [ css <| styleCell cell
        , onClick (OnPutStone cell.position)
        ]
        [ viewStone cell.stone (Set.member (Pos.toKey cell.position) cannotPutPositions)
        ]
    )


viewStone : Maybe Stone -> Bool -> Html Msg
viewStone mStone unPuttable =
    let
        blackColor =
            rgba 20 20 20 1

        whiteColor =
            rgba 230 230 230 1

        stoneStyle =
            [ borderRadius (pct 50)
            , boxSizing borderBox
            , Css.width (px stoneSize)
            , Css.height (px stoneSize)
            , display inlineBlock
            , transform <| translateY (px 4)
            ]

        textColor =
            color (rgba 150 150 150 0.8)

        fs =
            fontSize (px textSize)
    in
    case mStone of
        Nothing ->
            if unPuttable then
                span [ css [ textColor, fs ] ] [ text "×" ]

            else
                span
                    [ css
                        [ textColor
                        , fs
                        , display inlineBlock
                        , hover [ transform <| scale 1.3 ]
                        , Transitions.transition [ Transitions.transform2 100 0 ]
                        ]
                    ]
                    [ text "□" ]

        Just Black ->
            div
                [ css <|
                    [ border3 (px 1) solid whiteColor
                    , backgroundColor blackColor
                    ]
                        ++ stoneStyle
                ]
                []

        Just White ->
            div
                [ css <|
                    [ border3 (px 1) solid blackColor
                    , backgroundColor whiteColor
                    ]
                        ++ stoneStyle
                ]
                []


styleScore : List Style
styleScore =
    [ display inlineBlock
    , paddingLeft (px 32)
    ]


cellSize : Float
cellSize =
    40


stoneSize : Float
stoneSize =
    cellSize - 10


textSize : Float
textSize =
    24


styleCell : Cell -> List Style
styleCell cell =
    let
        bw =
            px 1

        bs =
            solid

        bc =
            rgba 200 200 200 0.3
    in
    [ Css.width (px cellSize)
    , Css.height (px cellSize)
    , boxSizing borderBox
    , display inlineBlock
    , textAlign center
    , fontSize (pct 150)
    , cursor pointer
    , borderBottom3 bw bs bc
    , borderRight3 bw bs bc
    , backgroundColor (rgba 0 50 0 1)
    ]
        ++ (if cell.position.x == 0 then
                [ borderLeft3 bw bs bc ]

            else
                []
           )
        ++ (if cell.position.y == 0 then
                [ borderTop3 bw bs bc ]

            else
                []
           )
        ++ (case cell.stone of
                Nothing ->
                    [ color (rgba 150 150 150 0.5) ]

                _ ->
                    []
           )


toStoneText : Maybe Stone -> String
toStoneText stone =
    case stone of
        Nothing ->
            "□"

        _ ->
            "●"


linkToRule : Html Msg
linkToRule =
    let
        s =
            "https://www.othello.org/lesson/lesson/rule.html"
    in
    a [ href s, Attr.target "_blank" ] [ text s ]
