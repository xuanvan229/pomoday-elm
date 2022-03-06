module Model exposing (..)
import Url
import Browser.Navigation as Nav

type alias Group = 
  {
    name: String,
    todo: List Todo
  }

type alias Todo =
  { title : String
  , completed : Bool
  }

type alias TodoCreate = 
  {
    title : String,
    group : String
  }


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , char : Char
  , text: String
  , show : Bool
  , groups : List Group
  , todos: List Todo
  }
