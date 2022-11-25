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
      RatingKeys: string list
      RatingMap: Map<string, int>
    }

    type Msg =
        | CostChange       of float
        | RatingChange     of int * int * int

    let init () = {
      Cost = 0.0
      Rating = {
          Responsive = 5
          Friendly   = 5
          Overall    = 5
      }
      FuzzyServiceRating = {
          Responsive = FuzzyVariable.grade 5.0 1.0 10.0
          Friendly   = FuzzyVariable.grade 5.0 1.0 10.0
          Overall    = FuzzyVariable.grade 5.0 1.0 10.0
      }
      
      RatingKeys = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"]
      RatingMap = Map.ofList [
        ("1", 1)
        ("2", 2)
        ("3", 3)
        ("4", 4)
        ("5", 5)
        ("6", 6)
        ("7", 7)
        ("8", 8)
        ("9", 9)
        ("10", 10)
      ]
    }

    let update msg model =
        match msg with
        | CostChange       cost ->  { model with Cost   = cost }
        | RatingChange(responsive, friendly, overall) -> {
          model with
            Rating = { Responsive = responsive; Friendly = friendly; Overall = overall }
            FuzzyServiceRating = {
              Responsive = FuzzyVariable.grade (float responsive) 1.0 10.0
              Overall    = FuzzyVariable.grade (float overall)    1.0 10.0
              Friendly   = FuzzyVariable.grade (float friendly)   1.0 10.0
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
                        Picker(
                          model.RatingKeys,
                          model.Rating.Responsive - 1,
                          fun (value: int) -> RatingChange(
                            model.RatingMap.[model.RatingKeys.[value]],
                            model.Rating.Friendly,
                            model.Rating.Overall
                          )
                        )
                        
                        Label("Was the server friendly? (1-10)").centerTextHorizontal()
                        
                        Picker(
                          model.RatingKeys,
                          model.Rating.Friendly - 1,
                          fun (value: int) -> RatingChange(
                            model.Rating.Responsive,
                            model.RatingMap.[model.RatingKeys.[value]],
                            model.Rating.Overall
                          )
                        )
                        
                        Label("How was your overall experience? (1-10)").centerTextHorizontal()
                        
                        Picker(
                          model.RatingKeys,
                          model.Rating.Overall - 1,
                          fun (value: int) -> RatingChange(
                            model.Rating.Responsive,
                            model.Rating.Friendly,
                            model.RatingMap.[model.RatingKeys.[value]]
                          )
                        )
                        
                        Label("Fuzzy Service Rating").centerTextHorizontal()
                        
                        Label(
                          sprintf
                            "Responsive(low = %.2f, med = %.2f, high = %.2f)"
                            model.FuzzyServiceRating.Responsive.Low
                            model.FuzzyServiceRating.Responsive.Med
                            model.FuzzyServiceRating.Responsive.High
                        )
                        
                        Label(
                          sprintf
                            "Friendly(low = %.2f, med = %.2f, high = %.2f)"
                            model.FuzzyServiceRating.Friendly.Low
                            model.FuzzyServiceRating.Friendly.Med
                            model.FuzzyServiceRating.Friendly.High
                        )
                        
                        Label(
                          sprintf
                            "Overall(low = %.2f, med = %.2f, high = %.2f)"
                            model.FuzzyServiceRating.Overall.Low
                            model.FuzzyServiceRating.Overall.Med
                            model.FuzzyServiceRating.Overall.High
                        )
                     }).centerVertical(expand = true)
                }
            )
        )

    let program = Program.stateful init update view
