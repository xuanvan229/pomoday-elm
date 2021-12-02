module Model exposing (..)
import Url
import Browser.Navigation as Nav



type alias Todo =
  { title : String
  , completed : Bool
  }


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , char : Char
  , text: String
  , show : Bool
  , todos: List Todo
  }