module Position exposing (..)


type alias Position =
    { x : Int, y : Int }


toKey : Position -> String
toKey pos =
    "(" ++ String.fromInt pos.x ++ "," ++ String.fromInt pos.y ++ ")"


up : Position -> Maybe Position
up { x, y } =
    if y == 0 then
        Nothing

    else
        Just { x = x, y = y - 1 }


down : Position -> Maybe Position
down { x, y } =
    if y == 7 then
        Nothing

    else
        Just { x = x, y = y + 1 }


right : Position -> Maybe Position
right { x, y } =
    if x == 7 then
        Nothing

    else
        Just { x = x + 1, y = y }


left : Position -> Maybe Position
left { x, y } =
    if x == 0 then
        Nothing

    else
        Just { x = x - 1, y = y }


upRight : Position -> Maybe Position
upRight { x, y } =
    if y == 0 || x == 7 then
        Nothing

    else
        Just { x = x + 1, y = y - 1 }


upLeft : Position -> Maybe Position
upLeft { x, y } =
    if y == 0 || x == 0 then
        Nothing

    else
        Just { x = x - 1, y = y - 1 }


downRight : Position -> Maybe Position
downRight { x, y } =
    if y == 7 || x == 7 then
        Nothing

    else
        Just { x = x + 1, y = y + 1 }


downLeft : Position -> Maybe Position
downLeft { x, y } =
    if y == 7 || x == 0 then
        Nothing

    else
        Just { x = x - 1, y = y + 1 }
