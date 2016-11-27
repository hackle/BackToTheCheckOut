module Main2

open Domain

let applyAnyOfPricing pricing state = state

let rec applySomeOfPricing (pricing: SomeOfPricing) (state: PriceState) =
    let takeOneItem (stack: (Item * int<piece>) list option) (pItem: Item, pCnt: int<piece>) =
        match stack with
        | None -> None
        | Some stack' ->
            if stack' |> List.exists (fun (i, _) -> i = pItem) |> not 
            then None
            else
                let attempt = 
                    stack'
                    |> List.map (fun (sItem, sCnt) -> 
                                    if sItem = pItem 
                                        then sItem, sCnt - pCnt 
                                        else sItem, sCnt)
                if List.forall (fun (_, qty) -> qty >= 0<piece>) attempt
                then attempt |> List.filter (fun (_, qty) -> qty > 0<piece>) |> Some
                else None

    let applied = List.fold takeOneItem (Some state.Rest) pricing.Items

    match applied with
    | Some s' ->  applySomeOfPricing pricing { Rest = s'; TotalPrice = state.TotalPrice + pricing.Price }
    | None -> state
    

let applyPricing state pricing =
    match pricing with
    | AnyOf aop -> applyAnyOfPricing aop state
    | SomeOf sop -> applySomeOfPricing sop state

let calc pricings itemCodes =
    let items = 
        itemCodes
        |> List.map Item
        |> List.groupBy id
        |> List.map (fun (item, itemGroup) -> item, (List.length itemGroup) * 1<piece>)
    
    pricings 
    |> List.fold applyPricing { Rest = items; TotalPrice = 0<cent> }
    |> (fun st -> st.TotalPrice)