module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (..)
import Update exposing (Msg(..), Model, Space(..), Piece(..))
import Collage exposing (..)
import Element exposing (..)


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
        (drawLines :: (List.map drawMove model.game))
        |> Element.toHtml


drawLines : Form
drawLines =
    group
        [ traced (solid black) (segment ( -50, -150 ) ( -50, 150 ))
        , traced (solid black) (segment ( 50, -150 ) ( 50, 150 ))
        , traced (solid black) (segment ( -150, -50 ) ( 150, -50 ))
        , traced (solid black) (segment ( -150, 50 ) ( 150, 50 ))
        ]


drawMove : ( Space, Piece ) -> Form
drawMove ( space, piece ) =
    move (getOffset space) (renderPiece piece)


getOffset : Space -> ( Float, Float )
getOffset space =
    case space of
        UpperLeft ->
            ( -100, 100 )

        UpperMid ->
            ( 0, 100 )

        UpperRight ->
            ( 100, 100 )

        MidLeft ->
            ( -100, 0 )

        MidMid ->
            ( 0, 0 )

        MidRight ->
            ( 100, 0 )

        BottomLeft ->
            ( -100, -100 )

        BottomMid ->
            ( 0, -100 )

        BottomRight ->
            ( 100, -100 )


renderPiece : Piece -> Form
renderPiece piece =
    case piece of
        O ->
            outlined (solid blue) (circle 50)

        X ->
            group
                [ traced (solid red) (segment ( -50, -50 ) ( 50, 50 ))
                , traced (solid red) (segment ( 50, -50 ) ( -50, 50 ))
                ]
