module View exposing (..)

import Html exposing (..)
import Color exposing (..)
import Update exposing (Msg(..), Model, Piece(..))
import Collage exposing (..)
import Element exposing (..)
import Arithmetic exposing (isEven, isOdd)


view : Model -> Html Msg
view model =
    div []
        [ case model.winner of
            Nothing ->
                Html.text ""

            Just X ->
                h1 [] [ Html.text "X wins!" ]

            Just O ->
                h1 [] [ Html.text "O wins!" ]
        , game model
        ]


game : Model -> Html Msg
game model =
    collage 300
        300
        (drawLines :: (List.indexedMap drawMove model.game))
        |> Element.toHtml


drawLines : Form
drawLines =
    group
        [ traced (solid black) (segment ( -50, -150 ) ( -50, 150 ))
        , traced (solid black) (segment ( 50, -150 ) ( 50, 150 ))
        , traced (solid black) (segment ( -150, -50 ) ( 150, -50 ))
        , traced (solid black) (segment ( -150, 50 ) ( 150, 50 ))
        ]


drawMove : Int -> Int -> Form
drawMove i space =
    move (getOffset space) (renderPiece i)


getOffset : Int -> ( Float, Float )
getOffset space =
    case space of
        1 ->
            ( -100, 100 )

        2 ->
            ( 0, 100 )

        3 ->
            ( 100, 100 )

        4 ->
            ( -100, 0 )

        5 ->
            ( 0, 0 )

        6 ->
            ( 100, 0 )

        7 ->
            ( -100, -100 )

        8 ->
            ( 0, -100 )

        9 ->
            ( 100, -100 )

        _ ->
            ( 0, 0 )


renderPiece : Int -> Form
renderPiece i =
    if isEven i then
        outlined (solid blue) (circle 50)
    else
        group
            [ traced (solid red) (segment ( -50, -50 ) ( 50, 50 ))
            , traced (solid red) (segment ( 50, -50 ) ( -50, 50 ))
            ]
