module Tippers.FuzzyLogic.Program

open Tippers.FuzzyLogic

printfn "'Value', 'Low', 'Med', 'High'"

 
for i in 0 .. 100 do
    let fuzzy = FuzzyVariable.grade (float i) 0.0 100.0

    printfn "%f, %f, %f, %f" (float i) fuzzy.Low  fuzzy.Med fuzzy.High

