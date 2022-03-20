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
import Json.Encode exposing (int, string, bool, object)
import Json.Encode exposing (list)

port parseString : String -> Cmd msg
port saveTasks: String -> Cmd msg

encodeTasks : Todo -> Json.Encode.Value
encodeTasks todo = 
  object
        [ ( "id", int todo.id )
        , ( "title", Json.Encode.string todo.title )
        , ( "completed", bool todo.completed )
        , ( "starting", bool todo.starting )
        ]

encodeGroup: Group -> Json.Encode.Value
encodeGroup group = 
   object
        [ ( "name", Json.Encode.string group.name )
        ]

saveListTasks: List Group -> Cmd msg
saveListTasks group = 
     Json.Encode.list encodeGroup group
        |> Json.Encode.encode 0
        |> saveTasks

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
      ( { model | todos = model.todos ++ [ {id =  1,  title = text, completed = False, starting = False} ] }
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
    BeginTask list_id -> 
      let
        findItemMatch item = List.filter (\id -> id == item.id) list_id

        updateTodos item = if (List.length (findItemMatch item) == 1) then {item | starting = True, completed = False} else  item
        updateGroup group = {group | todos = List.map updateTodos group.todos}
        groups = List.map updateGroup model.groups
      in
      ( {model | groups = groups}, Cmd.none )
    FinishTask list_id -> 
       let
        findItemMatch item = List.filter (\id -> id == item.id) list_id
        updateTodos item = if (List.length (findItemMatch item) == 1) then {item | starting = False, completed = True} else  item
        updateGroup group = {group | todos = List.map updateTodos group.todos}
        groups = List.map updateGroup model.groups
      in
      ( {model | groups = groups}, Cmd.none )
    CreateTask jsonData -> 
      case decodeString todoCreate jsonData of
        Ok todo ->
          let 
            isContain item = if (item.name == todo.group) then True else False
            total =  model.total + 1
            updateGroup item = if (item.name == todo.group ) then {item | todos = item.todos ++ [ {id =  total,  title = todo.title, completed = False, starting = False} ]} else  item

            groups = if List.any isContain model.groups then List.map updateGroup model.groups else model.groups ++ [Group todo.group [{id =  total,  title = todo.title, completed = False, starting = False}]]
          in
            ( { model |groups = groups, total = total }, Cmd.none )
        Err error ->
          ( model , Cmd.none )
     



focusSearchBox : Cmd Msg
focusSearchBox =
    Task.attempt (\_ -> NoOp) (Dom.focus "input-box")

todoCreate : Decoder Model.TodoCreate
todoCreate =
  map2 Model.TodoCreate
    (field "title" Json.Decode.string)
    (field "group" Json.Decode.string)