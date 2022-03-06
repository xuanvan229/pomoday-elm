port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Browser.Dom as Dom
import Browser.Events as Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Html.Events exposing (onInput)
import Url
import Task
import Model exposing (Todo, Model)
import View exposing (view)
import Msg exposing (Msg(..))
import Update exposing (update)
import Update exposing (update)

-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url 'C' "" False [] [], Cmd.none)


-- SUBSCRIPTIONS


port getTaskParsed : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ Events.onKeyDown keyDecoder, getTaskParsed RecvMsg]



keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)

toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            PressedLetter char

        _ ->
            PressedString string

-- VIEW
