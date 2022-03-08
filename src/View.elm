module View exposing (..)
import Msg exposing (Msg)
import Model exposing (Model, Todo)
import Url
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Model exposing (Group)
import Html.Attributes exposing (classList)

view : Model -> Browser.Document Msg
view model =
  { title = "Pomoday Elm"
  , body =
  [
    div [ class "p-10 h-screen relative  "] [
      div [ class "absolute bottom-0 right-0 p-4 bg-gray-300 rounded"] [
        div [class "flex font-mono items-center text-sm"] [
          text "Type anything to open the input"
        ],
        div [class "flex font-mono items-center text-sm"] [
          text "Type `task @group <title>` to create new task in group"
        ],
        div [class "flex font-mono items-center text-sm"] [
          text "Type `begin <id>` to start a task"
        ],
          div [class "flex font-mono items-center text-sm"] [
          text "Type `check <id>` to finish a task"
        ]
      ],
      div [class "flex font-mono items-center text-2xl"] [
        renderInputModal(model)
      ],
      div [] [
        ul [] (List.map rendergroupTodo model.groups)
      ]
    ]
  ]
  }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]


rendergroupTodo: Group -> Html Msg
rendergroupTodo group =
  li [class "flex flex-col items-start mb-10"] [
    div [ class "flex items-center bg-gray-400 items-center px-2 py-1 rounded-md mb-4" ] [
      div [ class "mr-4 font-bold text-md" ] [ text (group.name) ],
      div [ ] [ text ("[" ++ (String.fromInt (List.length group.todos) ++ "]")) ]
    ],
    ul [] (List.map renderTodo group.todos)
  ]

renderTodo : Todo -> Html Msg
renderTodo todo =
  li [ class "todo p-2" ]
    [ 
      div [ class "flex items-center view" ]
      [
        span [ class "mr-2 text-gray-300 border-r pr-1"] [text (String.fromInt todo.id)],
        span [ 
          classList [
            ("w-4 h-4 border border-2 border-stone-800 flex mr-2", True),
            ("bg-red-200", todo.starting),
            ("bg-green-400", todo.completed)
          ]
          ][],
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