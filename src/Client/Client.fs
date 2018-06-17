module Client

open System
open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Shared

open Fulma


type State = Hangman option

type Action =
| Guess of string
| NewWord of string

type GameProgress =
| Playing
| Win
| Lost

let maxAttempts = 6

let init () : State * Cmd<Action> =
    let state = None
    let cmd = Cmd.ofMsg (NewWord "Foo")
    state, cmd

let update (action : Action) (state : State) : State * Cmd<Action> =
    let state' =
        match state, action with
        | _, NewWord w -> Some { word = (w.ToUpper ()); guesses = List.Empty }
        | Some state, Guess g when (g.Length = 1) -> Some { state with guesses = (g.Chars 0 |> Char.ToUpper) :: state.guesses }
        | Some state, Guess _ -> Some state
        | None, Guess _ -> None
    state', Cmd.none

let showWord = function
| Some state ->
    let word = 
        state.word
        |> Seq.toList
        |> Seq.map (fun letter -> if (state.guesses |> Seq.contains letter) then letter.ToString () else "_")
        |> String.concat " "

    sprintf "%s (DEBUG: %A)"
    <| word
    <| state.guesses
| None -> "Create new word!"

let showGameStatus = function
| Some state ->
    match maxAttempts - state.guesses.Length with
    | remainng when remainng > 1 -> sprintf "Guesses remaing: %d" remainng
    | 1 -> "Last attempt!"
    | _ -> "You have lost!"
| None -> "No word created yet..."

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let guessInput onGuess =
    Input.text [ Input.Props [
            Value ""
            OnKeyDown onGuess ] ]

let view (state : State) (dispatch : Action -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE - Hangman" ] ] ]

          Container.container [ ]
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str (showWord state) ] ]

                Columns.columns [ Columns.IsCentered ]
                    [ Column.column [ Column.Width (Screen.All, Column.Is2) ] [ guessInput (fun e -> dispatch (Guess (e.key) )) ] ]

                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str (showGameStatus state) ] ]

                Columns.columns [ Columns.IsCentered ]
                    [ Column.column [ Column.Width (Screen.All, Column.Is2) ] [ button "New word" (fun _ -> dispatch (NewWord "hangman")) ] ]
              ]
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
