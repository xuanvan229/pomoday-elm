port module Update exposing (..)
import Browser
import Msg exposing (Msg(..))
import Model exposing (Model, Group, Todo)
import Browser.Navigation as Nav
import Url
import Browser.Dom as Dom
import Task
import Json.Decode exposing (Decoder, decodeString, field, string)
import Json.Decode exposing (map2)

port parseString : String -> Cmd msg

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
        ( { model | show = False, text = "" }, parseString model.text )
      else
        ( { model | show = True }, Cmd.none )
    RecvMsg textRecv -> 
      case decodeString todoCreate textRecv of
        Ok todo ->
          let 
            isContain item = if (item.name == todo.group) then True else False
            updateGroup item = if (item.name == todo.group ) then {item | todos = item.todos ++ [Todo todo.title False]} else  item
            groups = if List.any isContain model.groups then List.map updateGroup model.groups else model.groups ++ [Group todo.group [Todo todo.title False]]
          in
            ( { model | todos = model.todos ++ [Todo textRecv False], groups = groups }, Cmd.none )
        Err error ->
          ( { model | todos = model.todos ++ [Todo textRecv False] }, Cmd.none )



focusSearchBox : Cmd Msg
focusSearchBox =
    Task.attempt (\_ -> NoOp) (Dom.focus "input-box")

todoCreate : Decoder Model.TodoCreate
todoCreate =
  map2 Model.TodoCreate
    (field "title" string)
    (field "group" string)