import Browser
import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (type_, style, placeholder, value, rel, href, class)
import Html.Events exposing (onInput, onClick)

main =
    Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    {
        newTodo : String,
        todos : List Todo,
        idTracker : Int
    }

type alias Todo =
    {
        todoText : String,
        todoId : Int
    }

init : Model
init =
    {
        newTodo = "",
        todos = [],
        idTracker = 0
    }

type Msg
    = AddTodo
    | UpdateText String
    | DeleteTodo Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            {model | todos = List.sortBy .todoText ({ todoText = model.newTodo, todoId = model.idTracker } :: model.todos), newTodo = "", idTracker = model.idTracker + 1}
        UpdateText fieldText ->
            {model | newTodo = fieldText}
        DeleteTodo todoId ->
            {model | todos = List.filter (\todo -> todo.todoId /= todoId) model.todos}


view : Model -> Html Msg
view model =
    div []
        [ input [ style "margin-bottom" "5px", onInput UpdateText , placeholder "Add Todo", value model.newTodo] []
        , button [ onClick AddTodo ] [ text "Add"]
        , div [] (List.map renderTodo model.todos)
        ]

renderTodo : Todo -> Html Msg
renderTodo todo =
    div [ style "display" "flex", style "justify-content" "space-between", style "width" "300px", style "border" "1px solid black", style "padding" "5px", style "margin-bottom" "5px"] 
        [ div [] [ text todo.todoText ] 
        , div [ onClick (DeleteTodo todo.todoId) ] [text "X"]
        ]