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
      Rule1: float
      Rule2: float
      Rule3: float
      
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
      Rule1 = 0.0
      Rule2 = 0.0
      Rule3 = 0.0
    }

    let update msg model =
        match msg with
        | CostChange       cost ->  { model with Cost   = cost }
        | RatingChange(responsive, friendly, overall) ->
          
          let updatedResponsive = FuzzyVariable.grade (float responsive) 1.0 10.0
          let updatedOverall    = FuzzyVariable.grade (float overall)    1.0 10.0
          let updatedFriendly   = FuzzyVariable.grade (float friendly)   1.0 10.0
          
          let mediumOverall = FuzzyValue.FuzzyValue(updatedOverall.Med)
          let highOverall = FuzzyValue.FuzzyValue(updatedOverall.High)
          
          let veryFast = FuzzyValue.FuzzyValue(updatedResponsive.High)
          let veryFriendly = FuzzyValue.FuzzyValue(updatedFriendly.High)
          let veryOverall = FuzzyValue.FuzzyValue(updatedOverall.High)
          
          let rule1 = FuzzyValue.fuzzyAnd veryFast veryFriendly
          let rule2 = FuzzyValue.fuzzyOr mediumOverall highOverall
          let rule3 = FuzzyValue.fuzzyAnd (FuzzyValue.fuzzyAnd veryFast veryFriendly) veryOverall
          
          {          
            model with
              Rating = { Responsive = responsive; Friendly = friendly; Overall = overall }
              FuzzyServiceRating = {
                Responsive = updatedResponsive
                Overall    = updatedOverall
                Friendly   = updatedFriendly
              }
              
              Rule1 = match rule1 with | FuzzyValue.FuzzyValue(value) -> value
              Rule2 = match rule2 with | FuzzyValue.FuzzyValue(value) -> value
              Rule3 = match rule3 with | FuzzyValue.FuzzyValue(value) -> value
              
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
                        
                        Label("Fuzzy Rule Values").centerTextHorizontal()
                        
                        Label(
                          sprintf "Very fast and very friendly = %.2f" model.Rule1
                        )
                        
                        Label(
                          sprintf "Medium or great overall = %.2f" model.Rule2
                        )
                        
                        Label(
                          sprintf "Perfect = %.2f" model.Rule3
                        )
                        
                     }).centerVertical(expand = true)
                }
            )
        )

    let program = Program.stateful init update view
