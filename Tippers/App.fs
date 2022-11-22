namespace Tippers

open Xamarin.Forms
open Fabulous.XamarinForms

open type View

module App =
    type Model = { Count: int; Label: string }

    type Msg =
        | Increment
        | Decrement
        | TextChange of string

    let init () = { Count = 0; Label = "" }

    let update msg model =
        match msg with
        | Increment -> { model with Count = model.Count + 1 }
        | Decrement -> { model with Count = model.Count - 1 }
        | TextChange(text) -> { model with Label = text}
    let view model =
        Application(
            ContentPage(
                "Tippy",
                VStack() {
                    Label(model.Label)
                        .font(namedSize = NamedSize.Title)
                        .centerTextHorizontal()

                    (VStack() {
                        Label($"Count is {model.Count}")
                            .centerTextHorizontal()

                        Button("Increment", Increment)
                        Button("Decrement", Decrement)
                        Entry(model.Label, fun (txt) -> TextChange(txt))
                     })
                        .centerVertical(expand = true)
                }
            )
        )

    let program = Program.stateful init update view
