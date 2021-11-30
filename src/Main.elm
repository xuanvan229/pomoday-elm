module Main exposing (..)

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



type alias Todo =
  { title : String
  , completed : Bool
  }
-- MODEL


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , char : Char
  , text: String
  , show : Bool
  , todos: List Todo
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url 'C' "" False [], Cmd.none)



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | PressedLetter Char
  | PressedString String
  | TodoAdded String
  | OnChange String
  | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )
    TodoAdded text ->
      ( { model | todos = model.todos ++ [ Todo text False ] }
      , Cmd.none
      )
    OnChange string -> 
      ( { model | text = string }
      , Cmd.none
      )
    PressedLetter letter ->
      ( { model | char = letter, show= True }, focusSearchBox )
    PressedString string ->
      if string == "Escape" then
        ( { model | show = False }, Cmd.none )
      else if string == "Enter" then
        ( { model | todos = model.todos ++ [Todo model.text False], show = False, text = "" }, Cmd.none )
      else
        ( { model | show = True }, Cmd.none )



focusSearchBox : Cmd Msg
focusSearchBox =
    Task.attempt (\_ -> NoOp) (Dom.focus "input-box")

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Events.onKeyDown keyDecoder



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

onChangeValue : String -> Msg
onChangeValue value =
    OnChange value
-- VIEW


renderTodo : Todo -> Html Msg
renderTodo todo =
  li [ class "todo" ]
    [ 
      div [ class "view" ]
      [
        input [type_ "checkbox"] [],
        label [] [ text todo.title ]
      ]
    ]


renderInputModal : Model -> Html Msg
renderInputModal model =
  if model.show
    then
      div [
        class "fixed top-0 left-0 right-0 bottom-0 flex justify-center items-center"
      ] [
        div [class "w-9/12 p-2 bg-white rounded-md shadow-lg bg-gray-100 border-indigo-400	"] [
          input [onInput OnChange ,  id "input-box", class "w-full text-sm bg-transparent p-2 outline-none", placeholder "Type anything here...", autofocus True] []
        ]
      ]
    else
      div [] []

view : Model -> Browser.Document Msg
view model =
  { title = "Pomoday Elm"
  , body =
  [
    div [class "flex font-mono items-center text-2xl mt-10"] [
      renderInputModal(model),
      h1 [ class "font-bold mr-4" ] [text "Pomoday elm: "] ,
      div [ ] [ text (String.fromChar model.char) ],
      br [] []
    ],
    div [] [
      div [ ] [ text (model.text) ]
    ],
    div [] [
      div [ ] [ text (String.fromInt (List.length model.todos) ) ]
    ],
    div [] [
      ul [] (List.map renderTodo model.todos)
    ]
  ]
  }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]