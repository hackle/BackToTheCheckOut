module Main2

open Domain

let rec applyAnyOfPricing (pricing: AnyOfPricing) (state: PriceState) =
    let summer (i, q) = if List.contains i pricing.ChooseFrom then q else 0<piece>

    let rec takeOnce (acc: int<piece>) (items: (Item * int<piece>) list) =
        match acc = pricing.Quantity with
        | true -> items
        | false when List.sumBy summer items < pricing.Quantity - acc -> items
        | false ->
            let found = items |> List.find (fun (i, _) -> List.contains i pricing.ChooseFrom)
            items
            |> List.map (fun (i, q) -> if i = fst found then i, q - 1<piece> else (i, q))
            |> List.filter (fun (_, q) -> q > 0<piece>)
            |> takeOnce (acc + 1<piece>)

    match List.sumBy summer state.Rest < pricing.Quantity with
    | true -> state
    | false -> applyAnyOfPricing pricing { Rest = takeOnce 0<piece> state.Rest; TotalPrice = state.TotalPrice + pricing.Price }

let rec applySomeOfPricing (pricing: SomeOfPricing) (state: PriceState) =
    let takeOneItem (stack: (Item * int<piece>) list option) (pItem: Item, pCnt: int<piece>) =    
        let takeIfMatch (sItem, sCnt) = 
            match sItem = pItem with
            | true -> sItem, sCnt - pCnt 
            | false -> sItem, sCnt

        match stack with
        | None -> None
        | Some stack' when stack' |> List.exists (fun (i, _) -> i = pItem) |> not -> None
        | Some stack' ->
            let attempt = stack' |> List.map takeIfMatch
            match List.forall (fun (_, qty) -> qty >= 0<piece>) attempt with
            | false -> None
            | true -> attempt |> List.filter (fun (_, qty) -> qty > 0<piece>) |> Some

    let applied = List.fold takeOneItem (Some state.Rest) pricing.Items

    match applied with
    | None -> state
    | Some s' -> applySomeOfPricing pricing { Rest = s'; TotalPrice = state.TotalPrice + pricing.Price }
    
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