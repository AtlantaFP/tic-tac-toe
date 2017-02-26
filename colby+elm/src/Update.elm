module Update exposing (..)

import Mouse exposing (Position)
import Random


type Msg
    = NoOp
    | Move Position
    | MoveO Space


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
    { game = [ ( UpperRight, X ), ( BottomLeft, O ) ]
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
                else
                    updated ! [ moveO updated.game ]

        MoveO space ->
            (move model space O) ! []


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


moveO : Game -> Cmd Msg
moveO game =
    let
        avail =
            List.filter ((spaceTaken game) >> not) allSpaces

        len =
            List.length avail
    in
        if ((List.length game) == 9) then
            Cmd.none
        else
            Random.generate (MoveO)
                (Random.map
                    (\i ->
                        case (getAt avail i) of
                            Nothing ->
                                UpperLeft

                            Just space ->
                                space
                    )
                    (Random.int 0 len)
                )


getAt : List a -> Int -> Maybe a
getAt list i =
    list
        |> List.drop (i - 1)
        |> List.head


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
