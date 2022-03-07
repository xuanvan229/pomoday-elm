module Msg exposing (..)
import Url
import Browser


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | PressedLetter Char
  | PressedString String
  | TodoAdded String
  | OnChange String
  | CreateTask String
  | BeginTask Int
  | NoOp