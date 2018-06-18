module Client

open System
open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Fulma

type Hangman =
    {
        word: string
        guesses: char list
    }

type State = Hangman option

type GameProgress =
| Playing of int
| PlayingLastAttempt
| Win
| Lost

type Action =
| Guess of string
| NewWord of string

let maxAttempts = 6

let init () : State * Cmd<Action> =
    let state = None
    let cmd = Cmd.ofMsg (NewWord "Amalka")
    state, cmd

let guessChar (guess: string) =
    (guess.Chars 0) |> Char.ToUpper

let update (action : Action) (state : State) : State * Cmd<Action> =
    let state' =
        match state, action with
        | _, NewWord w -> Some { word = (w.ToUpper ()); guesses = List.Empty }
        | Some state, Guess g when (g.Length = 1) && state.guesses |> List.contains (g |> guessChar) -> Some state
        | Some state, Guess g when (g.Length = 1) -> Some { state with guesses = (g |> guessChar) :: state.guesses }
        | Some state, Guess _ -> Some state
        | None, Guess _ -> None

    state', Cmd.none

let showWord = function
| Some state ->
    state.word
    |> Seq.toList
    |> Seq.map (fun letter -> if (state.guesses |> Seq.contains letter) then letter.ToString () else "_")
    |> String.concat " "
| None -> "Create new word!"

let remainingLetters state =
    state.word
    |> Seq.toList
    |> List.filter ((fun l -> state.guesses |> List.contains l) >> not)
    |> List.length

let remainingAttempts state =
    let charToString c = c.ToString ()
    let wrongAttempts =
        state.guesses
        |> List.map charToString
        |> List.filter (state.word.Contains >> not)
        |> List.length

    maxAttempts - wrongAttempts

let gameStatus state =
    match (remainingLetters state, remainingAttempts state) with
    | remainingLetters, remainingAttempts when remainingLetters <= 0 && remainingAttempts > 0 -> Win
    | remainingLetters, remainingAttempts when remainingLetters > 0 && remainingAttempts = 1 -> PlayingLastAttempt
    | remainingLetters, remainingAttempts when remainingLetters > 0 && remainingAttempts > 1 -> Playing remainingAttempts
    | _ -> Lost

let showGameStatus = function
| Some state ->
    match state |> gameStatus with
    | Playing remaining -> sprintf "Guesses remaining: %d" remaining
    | PlayingLastAttempt -> sprintf "Last attempt!"
    | Win -> "You have guessed the word! Congratulations!"
    | Lost -> "You have lost!"
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
    let guessInputColumn =
        Columns.columns [ Columns.IsCentered ]
                    [ Column.column [ Column.Width (Screen.All, Column.Is2) ] [ guessInput (fun e -> dispatch (Guess (e.key) )) ] ]

    let inputColumn =
        match state with
        | Some state ->
            match state |> gameStatus with
            | Playing _ -> guessInputColumn
            | PlayingLastAttempt -> guessInputColumn
            | _ -> null
        | _ -> null

    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE - Hangman" ] ] ]

          Container.container [ ]
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str (showWord state) ] ]

                inputColumn

                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str (showGameStatus state) ] ]

                Columns.columns [ Columns.IsCentered ]
                    [ Column.column [ Column.Width (Screen.All, Column.Is2) ] [ button "New word" (fun _ -> dispatch (NewWord "Beira")) ] ]
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
