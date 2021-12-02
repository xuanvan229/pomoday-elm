module Update exposing (..)
import Browser
import Msg exposing (Msg(..))
import Model exposing (Model, Todo)
import Browser.Navigation as Nav
import Url
import Browser.Dom as Dom
import Task

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
