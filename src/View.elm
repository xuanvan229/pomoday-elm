module View exposing (..)
import Msg exposing (Msg)
import Model exposing (Model, Todo)
import Url
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


view : Model -> Browser.Document Msg
view model =
  { title = "Pomoday Elm"
  , body =
  [
    div [ class "p-10 h-screen"] [
      div [class "flex font-mono items-center text-2xl"] [
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
  ]
  }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]


renderTodo : Todo -> Html Msg
renderTodo todo =
  li [ class "todo" ]
    [ 
      div [ class "flex items-center view" ]
      [
        span [ class "w-4 h-4 border border-2 border-stone-800 flex mr-2"][],
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
          input [onInput Msg.OnChange ,  id "input-box", class "w-full text-sm bg-transparent p-2 outline-none", placeholder "Type anything here...", autofocus True] []
        ]
      ]
    else
      div [] []