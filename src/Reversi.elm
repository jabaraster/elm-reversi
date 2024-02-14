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
                    Json.Decode.decodeString decodeModel (Debug.log "" value)
                        |> Result.withDefault model
            in
            ( { m | message = "復元しました" }, Cmd.none )


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
                ++ (if List.isEmpty puttableState.puttable then
                        if (getPuttableState (reverse model.turn) model.board).puttable |> List.isEmpty then
                            [ viewGameOver ]

                        else
                            [ viewPass ]

                    else
                        []
                   )
                ++ [ div [ css [ fontSize (pct 150) ] ]
                        [ div [ css styleScore ] [ viewStone (Just Black) True, text ":", text <| String.fromInt cnt.black ]
                        , div [ css styleScore ] [ viewStone (Just White) True, text ":", text <| String.fromInt cnt.white ]
                        ]
                   , div [] [ text "手番 : ", viewStone (Just model.turn) True ]
                   , div []
                        [ text <|
                            model.message
                                ++ "[置けるセルの数："
                                ++ (String.fromInt <| List.length puttableState.puttable)
                                ++ "][置けないセルの数："
                                ++ (String.fromInt <| List.length puttableState.unputtable)
                                ++ "]"
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
            [ css [ padding (px 4) ]
            , onClick OnAgain
            ]
            [ text "もう１度遊ぶ" ]
        ]


viewPass : Html Msg
viewPass =
    viewInformation (rgba 255 255 255 0.3)
        [ p [] [ text "置ける場所がありません。" ]
        , button
            [ css
                [ fontSize (pct 130)
                , padding (px 4)
                ]
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
        [ span
            []
            [ viewStone cell.stone (Set.member (Pos.toKey cell.position) cannotPutPositions) ]
        ]
    )


viewStone : Maybe Stone -> Bool -> Html Msg
viewStone mStone unPuttable =
    let
        fs =
            fontSize (px 40)
    in
    case mStone of
        Nothing ->
            if unPuttable then
                span [ css [ color (rgba 150 150 150 0.8), fontSize (px 24) ] ] [ text "×" ]

            else
                span [ css [ color (rgba 150 150 150 0.8), fontSize (px 24) ] ] [ text "□" ]

        Just Black ->
            span
                [ css
                    [ Css.property "-webkit-text-stroke" "1px rgba(230, 230, 230, .8)"
                    , color (rgba 20 20 20 1)
                    , fs
                    ]
                ]
                [ text "●" ]

        Just White ->
            span
                [ css
                    [ Css.property "-webkit-text-stroke" "1px rgba(30, 30, 30, .8)"
                    , color (rgba 240 240 240 1)
                    , fs
                    ]
                ]
                [ text "●" ]


styleScore : List Style
styleScore =
    [ display inlineBlock
    , paddingLeft (px 32)
    ]


styleCell : Cell -> List Style
styleCell cell =
    [ Css.width (px 48)
    , Css.height (px 48)
    , display inlineBlock
    , textAlign center
    , fontSize (pct 200)
    , cursor pointer
    ]
        ++ (case cell.stone of
                Nothing ->
                    [ color (rgba 150 150 150 0.5) ]

                Just Black ->
                    [ textShadow4 (px 1) (px 2) (px 10) (rgba 255 255 255 0.3)
                    , color (rgba 20 20 20 1)
                    ]

                Just White ->
                    [ textShadow4 (px 1) (px 2) (px 10) (rgba 255 255 255 0.3)
                    , color (rgba 240 240 240 1)
                    ]
           )
        ++ (case cell.stone of
                Just _ ->
                    []

                Nothing ->
                    [ hover [ transform <| scale 1.2 ]
                    , Transitions.transition [ Transitions.transform2 100 0 ]
                    ]
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
