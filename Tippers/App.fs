namespace Tippers

open Xamarin.Forms
open Fabulous.XamarinForms

open type View

module App =
    type ServiceRating = {
      Responsive: int
      Overall: int
      Friendly: int
    }
    
    type Model = {
      Cost: float
      Rating: ServiceRating
      
    }

    type Msg =
        | CostChange of float
        | ResponsiveChange of int
        | FriendlyChange of int
        | OverallChange of int

    let init () = {
      Cost = 0.0
      Rating = {
          Responsive = 5
          Overall = 5
          Friendly = 5
      }
    }

    let update msg model =
        match msg with
        | CostChange cost -> { model with Cost = cost }
        | ResponsiveChange(value) -> {model with Rating = { model.Rating with Responsive = value}}
        | FriendlyChange(value) -> {model with Rating = {model.Rating with Friendly = value}}
        | OverallChange(value) -> {model with Rating = {model.Rating with Overall = value}}
    let view model =
        Application(
            ContentPage(
                "Tippers",
                VStack() {
                    Label("Tippers")
                        .font(namedSize = NamedSize.Title)
                        .centerTextHorizontal()

                    (VStack() {
                
                        Label("Total Cost").centerTextHorizontal()
                        Entry(model.Cost.ToString(), fun (value) -> CostChange(value |> float))
                        
                        Label("Was the server responsive? (1-10)").centerTextHorizontal()
                        
                        Entry(model.Rating.Responsive.ToString(), fun (txt) -> ResponsiveChange(txt |> int))
                        
                        Label("Was the server friendly? (1-10)").centerTextHorizontal()
                        Entry(model.Rating.Friendly.ToString(), fun (txt) -> FriendlyChange(txt |> int))
                        
                        Label("How was your overall experience? (1-10)").centerTextHorizontal()
                        Entry(model.Rating.Overall.ToString(), fun (txt) -> OverallChange(txt |> int))
                     })
                        .centerVertical(expand = true)
                }
            )
        )

    let program = Program.stateful init update view
