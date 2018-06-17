module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Shared

open Fulma


type State = Hangman option

type Action =
| Guess of char
| NewWord of string

let initialState = {
    word = ""
    guesses = 0
}

let init () : State * Cmd<Action> =
    let state = None
    let cmd = Cmd.ofMsg (NewWord "Foo")
    state, cmd

let update (action : Action) (state : State) : State * Cmd<Action> =
    let model' =
        match state, action with
        | _, NewWord w -> Some { word = w; guesses = 0 }
        | Some state, Guess x -> Some { state with word = state.word + x.ToString(); guesses = state.guesses + 1 } // todo fix
        | None, Guess _ -> None
    model', Cmd.none

let show = function
| Some state ->
    sprintf "%s (%d)" state.word state.guesses
| None -> "Click the button!"

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view (state : State) (dispatch : Action -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE - Hangman" ] ] ]

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Word: " + show state) ] ]
                Columns.columns []
                    [ Column.column [] [ button "-" (fun _ -> dispatch (Guess 'x')) ]
                      Column.column [] [ button "+" (fun _ -> dispatch (Guess 'y')) ] ] ]

        ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
