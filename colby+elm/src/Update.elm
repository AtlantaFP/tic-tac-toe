module Update exposing (..)

import Mouse exposing (Position)
import Arithmetic exposing (isEven, isOdd)


type Msg
    = NoOp
    | Move Position


type Piece
    = O
    | X


type alias Game =
    List Int


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
                    (move model space)
            in
                if List.member space model.game then
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


move : Model -> Int -> Model
move model space =
    let
        game =
            model.game ++ [ space ]

        winner =
            getWinner game
    in
        { model | game = game, winner = winner }


locateSpace : Position -> Int
locateSpace { x, y } =
    (+)
        (if x < 100 then
            1
         else if x < 200 then
            2
         else
            3
        )
        (if y < 100 then
            0
         else if y < 200 then
            3
         else
            6
        )


getWinner : Game -> Maybe Piece
getWinner game =
    if (checkWinner (ohs game)) then
        Just O
    else if (checkWinner (exes game)) then
        Just X
    else
        Nothing


exes : Game -> List Int
exes game =
    getAll isOdd game


ohs : Game -> List Int
ohs game =
    getAll isEven game


getAll : (Int -> Bool) -> Game -> List Int
getAll checker game =
    game
        |> List.indexedMap (,)
        |> List.filter (\m -> checker (Tuple.first m))
        |> List.map Tuple.second


checkWinner : List Int -> Bool
checkWinner spaces =
    List.any (isSubset spaces) winners


isSubset : List a -> List a -> Bool
isSubset set subset =
    List.all ((flip List.member) set) subset


winners : List (List Int)
winners =
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    , [ 7, 8, 9 ]
    , [ 1, 4, 7 ]
    , [ 2, 5, 8 ]
    , [ 3, 6, 9 ]
    , [ 1, 5, 9 ]
    , [ 3, 5, 7 ]
    ]



-- bot section


botMove : Model -> Model
botMove model =
    let
        avail =
            available model.game

        next =
            avail
                |> List.map (calcMove model)
                |> Debug.log "calculated"
                |> List.sortBy Tuple.second
                |> List.head
                |> Maybe.withDefault ( 1, 1 )
                |> Tuple.first
    in
        move model next


calcMove : Model -> Int -> ( Int, Float )
calcMove model space =
    let
        moved =
            move model space
    in
        case moved.winner of
            Just X ->
                ( space, 0 )

            Just O ->
                ( space, 1 )

            Nothing ->
                if List.length moved.game == 9 then
                    ( space, 0 )
                else
                    ( space
                    , available moved.game
                        |> List.map (calcMove moved)
                        |> List.map Tuple.second
                        |> List.sum
                        |> (*) 0.1
                    )


available : List Int -> List Int
available moves =
    List.range 1 9
        |> List.filter
            (\x ->
                not (List.member x moves)
            )
