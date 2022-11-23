namespace Tippers

open Tippers.FuzzyLogic
open Xamarin.Forms
open Fabulous.XamarinForms
open type View

module App =
    type ServiceRating = {
      Responsive: int
      Overall:    int
      Friendly:   int
    }
    
    type FuzzyServiceRating = {
      Responsive: FuzzyVariable.FuzzyVariable
      Overall:    FuzzyVariable.FuzzyVariable
      Friendly:   FuzzyVariable.FuzzyVariable
    }
    
    type Model = {
      Cost:   float
      Rating: ServiceRating
      FuzzyServiceRating: FuzzyServiceRating
    }

    type Msg =
        | CostChange       of float
        | RatingChange     of int * int * int

    let init () = {
      Cost = 0.0
      Rating = {
          Responsive = 1
          Friendly   = 3
          Overall    = 7
      }
      FuzzyServiceRating = {
          Responsive = FuzzyVariable.grade 1.0 0.0 10.0
          Friendly   = FuzzyVariable.grade 3.0 0.0 10.0
          Overall    = FuzzyVariable.grade 7.0 0.0 10.0
      }
    }

    let update msg model =
        match msg with
        | CostChange       cost ->  { model with Cost   = cost }
        | RatingChange(responsive, friendly, overall) -> {
          model with
            Rating = { Responsive = responsive; Friendly = friendly; Overall = overall }
            FuzzyServiceRating = {
              Responsive = FuzzyVariable.grade (float responsive) 0.0 10.0
              Overall    = FuzzyVariable.grade (float overall)    0.0 10.0
              Friendly   = FuzzyVariable.grade (float friendly)   0.0 10.0
            }
        }
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
                        Entry(
                          model.Cost.ToString(),
                          fun (value) -> CostChange(value |> float)
                        )
                        
                        Label("Was the server responsive? (1-10)").centerTextHorizontal() 
                        Entry(
                          model.Rating.Responsive.ToString(),
                          fun (txt) -> RatingChange(txt |> int, model.Rating.Friendly, model.Rating.Overall)
                        )
                        
                        Label("Was the server friendly? (1-10)").centerTextHorizontal()
                        Entry(
                          model.Rating.Friendly.ToString(),
                          fun (txt) -> RatingChange(model.Rating.Responsive, txt |> int, model.Rating.Overall)
                        )
                        
                        Label("How was your overall experience? (1-10)").centerTextHorizontal()
                        Entry(
                          model.Rating.Overall.ToString(),
                          fun (txt) -> RatingChange(model.Rating.Responsive, model.Rating.Friendly, txt |> int)
                        )
                        
                        Label("Fuzzy Service Rating").centerTextHorizontal()
                        
                        Label($"Responsive(low = {model.FuzzyServiceRating.Responsive.Low}, med = {model
                        .FuzzyServiceRating.Responsive.Med}, high = {model.FuzzyServiceRating.Responsive.High})"
                        )
                        
                        Label($"Friendly(low = {model.FuzzyServiceRating.Friendly.Low}, med = {model
                        .FuzzyServiceRating.Friendly.Med}, high = {model.FuzzyServiceRating.Friendly.High})"
                        )
                        Label($"Overall(low = {model.FuzzyServiceRating.Overall.Low}, med = {model
                        .FuzzyServiceRating.Overall.Med}, high = {model.FuzzyServiceRating.Overall.High})"
                        )
                     }).centerVertical(expand = true)
                }
            )
        )

    let program = Program.stateful init update view
