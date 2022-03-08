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
import Model exposing (Todo, Group, Model)
import View exposing (view)
import Msg exposing (Msg(..))
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


groups :  List Group
groups = [ Group "@personal" [  {id =  1,  title = "Buy milk", completed = False, starting = False} 
                              , {id =  2,  title = "Buy eggs", completed = False, starting = False} 
                              , {id =  3,  title = "Buy bread", completed = False, starting = False} 
                              ]
         , Group "@work" [    {id =  4,  title = "Buy milk", completed = False, starting = False} 
                            , {id =  5,  title = "Buy eggs", completed = False, starting = False} 
                            , {id =  6,  title = "Buy bread", completed = False, starting = False} 
                         ]
         ]

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url 'C' "" False groups [] 6, Cmd.none)


-- SUBSCRIPTIONS


port createNewTask : (String -> msg) -> Sub msg
port startATask : (Int -> msg) -> Sub msg
port finishATask: (Int -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch [ Events.onKeyDown keyDecoder, createNewTask CreateTask, startATask BeginTask, finishATask FinishTask ]



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
