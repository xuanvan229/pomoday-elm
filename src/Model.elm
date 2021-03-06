module Model exposing (..)
import Url
import Browser.Navigation as Nav

type alias Group = 
  {
    name: String,
    todos: List Todo
  }

type alias Todo =
  { 
    id: Int
  ,  title : String
  , completed : Bool
  , starting: Bool
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
  , total: Int
  }
