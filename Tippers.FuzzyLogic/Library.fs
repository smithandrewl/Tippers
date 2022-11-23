namespace Tippers.FuzzyLogic

module Primitives =
  let numericGrade(value: float) (lowerLimit: float) (lowerSupportLimit: float) =
    if value < lowerLimit then
      0.0
    elif(value >= lowerLimit) && (value <= lowerSupportLimit) then
      (value - lowerLimit) / (lowerSupportLimit - lowerLimit)
    else
      1.0
  
  let reverseGrade (value: float) (upperLimit: float) (upperSupportLimit: float) =
    if value > upperLimit then
      0.0
    elif(value >= upperSupportLimit) && (value <= upperLimit) then
      (upperLimit - value) / (upperLimit - upperSupportLimit)
    else
      1.0
      
  let triangleGrade (value: float) (lowerLimit: float) (middle: float) (upper: float) =
    if value <= lowerLimit then
      0.0
    elif lowerLimit<value && (value <= middle) then
      (value - lowerLimit) / (middle - lowerLimit)
    elif middle<value && (value < upper) then
      (upper - value) / (upper - middle)
    else
      0.0
module FuzzyValue =
  type FuzzyValue =
    | FuzzyValue of float
  
  let fuzzyAnd(a: FuzzyValue) (b: FuzzyValue) =
    match a, b with
    | FuzzyValue(a), FuzzyValue(b) -> FuzzyValue(min a b)
    
  let fuzzyOr(a: FuzzyValue) (b: FuzzyValue) =
    match a, b with
    | FuzzyValue(a), FuzzyValue(b) -> FuzzyValue(max a b)
    
  let fuzzyNot(a: FuzzyValue) =
    match a with
    | FuzzyValue(a) -> FuzzyValue(1.0 - a)
    
module FuzzyVariable =
  type FuzzyVariable = {
    Value: float
    Low: float
    Med: float
    High: float
  }
  
  let Value (fuzzyVariable: FuzzyVariable) =
    FuzzyValue.FuzzyValue(fuzzyVariable.Value)
  
  let Low (fuzzyVariable: FuzzyVariable) =
    FuzzyValue.FuzzyValue(fuzzyVariable.Low)
  
  let High (fuzzyVariable: FuzzyVariable) =
    FuzzyValue.FuzzyValue(fuzzyVariable.High)
  
  let create (low: float) (med: float) (high: float) =
    {
      Value = 0.0
      Low = low
      Med = med
      High = high
    }
  
  let grade(value: float) (low: float) (high: float) =
    let med = (high - low) / 2.0
    
    {
      Value = value
      Low = Primitives.reverseGrade value med low
      Med = Primitives.triangleGrade value low med high
      High = Primitives.numericGrade value med high
    }
    
  let defuzzify (variable: FuzzyVariable) (lowWeight: float) (medWeight: float) (highWeight: float) =
    let appliedTotal = (lowWeight * variable.Low) + (medWeight * variable.Med) + (highWeight * variable.High)
    
    let valueTotal = variable.Low + variable.Med + variable.High
    
    appliedTotal / valueTotal
  
  
  