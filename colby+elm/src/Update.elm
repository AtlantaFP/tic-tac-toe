module Update exposing (..)

import Mouse exposing (Position)


type Msg
    = NoOp
    | Move Position


type Space
    = UpperLeft
    | UpperMid
    | UpperRight
    | MidLeft
    | MidMid
    | MidRight
    | BottomLeft
    | BottomMid
    | BottomRight


type Piece
    = X
    | O


type alias Game =
    List ( Space, Piece )


type alias Model =
    { game : Game
    , winner : Maybe Piece
    }


init : ( Model, Cmd Msg )
init =
    { game = []
    , winner = Nothing
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Move position ->
            let
                space =
                    locateSpace position

                updated =
                    (move model space X)
            in
                if spaceTaken model.game space then
                    model ! []
                else if (updated.winner /= Nothing) then
                    updated ! []
                else if (List.length updated.game) == 9 then
                    updated ! []
                else
                    botMove updated ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.clicks Move


move : Model -> Space -> Piece -> Model
move model space piece =
    let
        game =
            ( space, piece ) :: model.game

        winner =
            getWinner game
    in
        { model | game = game, winner = winner }


spaceTaken : Game -> Space -> Bool
spaceTaken game space =
    game
        |> List.map Tuple.first
        |> List.member space


locateSpace : Position -> Space
locateSpace { x, y } =
    if x < 100 then
        leftSide y
    else if x < 200 then
        middle y
    else
        rightSide y


leftSide : Int -> Space
leftSide y =
    if y < 100 then
        UpperLeft
    else if y < 200 then
        MidLeft
    else
        BottomLeft


middle : Int -> Space
middle y =
    if y < 100 then
        UpperMid
    else if y < 200 then
        MidMid
    else
        BottomMid


rightSide : Int -> Space
rightSide y =
    if y < 100 then
        UpperRight
    else if y < 200 then
        MidRight
    else
        BottomRight


getWinner : Game -> Maybe Piece
getWinner game =
    let
        exes =
            getAll game X

        ohs =
            getAll game O
    in
        if (checkWinner exes) then
            Just X
        else if (checkWinner ohs) then
            Just O
        else
            Nothing


getAll : Game -> Piece -> List Space
getAll game piece =
    game
        |> List.filter (\x -> piece == (Tuple.second x))
        |> List.map Tuple.first


checkWinner : List Space -> Bool
checkWinner spaces =
    List.any (isSubset spaces) winners


isSubset : List a -> List a -> Bool
isSubset set subset =
    List.all (\p -> List.member p set) subset


winners : List (List Space)
winners =
    [ [ UpperLeft, UpperMid, UpperRight ]
    , [ MidLeft, MidMid, MidRight ]
    , [ BottomLeft, BottomMid, BottomRight ]
    , [ UpperLeft, MidLeft, BottomLeft ]
    , [ UpperMid, MidMid, BottomMid ]
    , [ UpperRight, MidRight, BottomRight ]
    , [ UpperLeft, MidMid, BottomRight ]
    , [ UpperRight, MidMid, BottomLeft ]
    ]



-- bot moves


botMove : Model -> Model
botMove model =
    let
        space =
            calculate model
    in
        case space of
            Nothing ->
                model

            Just space ->
                move model space O


calculate : Model -> Maybe Space
calculate model =
    getAvailable model.game
        |> List.map (calcScore model)
        |> List.sortBy Tuple.second
        |> List.head
        |> Maybe.map Tuple.first


calcScore : Model -> Space -> ( Space, Int )
calcScore model space =
    let
        updated =
            move model space O
    in
        case updated.winner of
            Just X ->
                ( space, -3 )

            Just O ->
                ( space, 2 )

            Nothing ->
                if (List.length updated.game) == 9 then
                    ( space, 0 )
                else
                    ( space
                    , getAvailable updated.game
                        |> List.map (calcScore updated)
                        |> List.map Tuple.second
                        |> List.sum
                    )


getAvailable : Game -> List Space
getAvailable game =
    List.filter ((spaceTaken game) >> not) allSpaces


allSpaces : List Space
allSpaces =
    [ UpperLeft
    , UpperMid
    , UpperRight
    , MidLeft
    , MidMid
    , MidRight
    , BottomLeft
    , BottomMid
    , BottomRight
    ]
