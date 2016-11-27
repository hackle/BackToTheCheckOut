module Main2

open Domain

type PriceState = { Rest: Item list; TotalPrice: int<cent> }

let applyAnyOfPricing pricing state = state
let applySomeOfPricing pricing state = state

let applyPricing state pricing =
    match pricing with
    | AnyOf aop -> applyAnyOfPricing aop state
    | SomeOf sop -> applySomeOfPricing sop state

let calc pricings itemCodes =
    let items = 
        itemCodes
        |> List.map Item
    
    pricings 
    |> List.fold applyPricing { Rest = items; TotalPrice = 0<cent> }
    |> (fun st -> st.TotalPrice)