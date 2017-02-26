module Main exposing (..)

import View exposing (view)
import Html
import Update exposing (subscriptions, update, init, Model, Msg)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
