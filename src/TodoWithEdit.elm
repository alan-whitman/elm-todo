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
        todoId : Int,
        editable : Bool
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
    | UpdateNewTodo String
    | UpdateTodo Int String
    | ToggleEditable Int
    | DeleteTodo Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            if model.newTodo /= "" then
                {model | todos = List.sortBy .todoText ({ todoText = model.newTodo, todoId = model.idTracker, editable = False } :: model.todos), newTodo = "", idTracker = model.idTracker + 1}
            else
                model
        UpdateNewTodo fieldText ->
            {model | newTodo = fieldText}
        UpdateTodo todoId fieldText  ->
            let
                updateTodoText : Todo -> Todo
                updateTodoText todo =
                    if todo.todoId == todoId then
                        { todo | todoText = fieldText}
                    else
                        todo
                todos = List.map updateTodoText model.todos
            in
                {model | todos = todos}
        DeleteTodo todoId ->
            {model | todos = List.filter (\todo -> todo.todoId /= todoId) model.todos}
        ToggleEditable todoId ->
            let
                updateEditableStatus : Todo -> Todo
                updateEditableStatus todo =
                    if todo.todoId == todoId then
                        { todo | editable = not todo.editable }
                    else
                        todo
                todos = List.map updateEditableStatus model.todos
            in
                { model | todos = List.sortBy .todoText todos }



view : Model -> Html Msg
view model =
    div []
        [ input [ style "margin-bottom" "5px", onInput UpdateNewTodo , placeholder "Add Todo", value model.newTodo] []
        , button [ onClick AddTodo ] [ text "Add"]
        , div [] (List.map renderTodo model.todos)
        ]

renderTodo : Todo -> Html Msg
renderTodo todo =
    if not todo.editable then
        div [ style "display" "flex", style "justify-content" "space-between", style "align-items" "center", style "width" "300px", style "border" "1px solid black", style "padding" "5px", style "margin-bottom" "5px", style "height" "25px"]
            [ div [ style "width" "240px"] [ text todo.todoText ] 
            , div [ onClick (ToggleEditable todo.todoId) ] [ text "Edit"]
            , div [ onClick (DeleteTodo todo.todoId) ] [text "X"] 
            ]
    else
        div [ style "display" "flex", style "justify-content" "space-between", style "align-items" "center", style "width" "300px", style "border" "1px solid black", style "padding" "5px", style "margin-bottom" "5px", style "height" "25px"]
            [ input [ onInput (UpdateTodo todo.todoId ), style "width" "240px", value todo.todoText ] []
            , div [ onClick (ToggleEditable todo.todoId) ] [ text "Save"]
            , div [ onClick (DeleteTodo todo.todoId) ] [text "X"] 
            ]