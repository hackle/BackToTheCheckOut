module Main

open Domain

type PriceState = { Rest: (Item * int<piece>) list; TotalPrice: int<cent> }

let rec applySomeOfPricingOnce (pricing: SomeOfPricing) (priceState: PriceState) = 
    let { Rest = rest; TotalPrice = totalPrice } = priceState
    
    let isBuyingEnoughForPricing (pItem, pQuantity) =
        let isEnoughAndMatching (bItem, bQuantity) = bItem = pItem && bQuantity >= pQuantity
        List.exists isEnoughAndMatching rest

    let pricingCanApply = List.forall isBuyingEnoughForPricing pricing.Items

    match pricingCanApply with
    | false -> priceState
    | true ->
        let pricingItemsMap = dict pricing.Items
        let takeApplicableItems (itemLeft, quantityLeft) =
            match pricingItemsMap.ContainsKey itemLeft with
            | true -> itemLeft, quantityLeft - pricingItemsMap.Item(itemLeft)
            | false -> itemLeft, quantityLeft

        let rest' = List.map takeApplicableItems rest
        applySomeOfPricingOnce pricing { Rest = rest'; TotalPrice = totalPrice + pricing.Price }

let rec applyAnyOfPricingOnce (pricing: AnyOfPricing) (priceState: PriceState) =
    let { ChooseFrom = itemsToChooseFrom; Quantity = cntNeededForPricing; Price = price } = pricing
    let { Rest = itemsUnpaid; TotalPrice = total } = priceState

    let getCntBuyable (bItem, bQuantity) =
       match List.exists ((=) bItem) itemsToChooseFrom with 
       | true -> bQuantity 
       | false -> 0<piece>

    let rec takeOneLot (cntTaken, rest') (pItem, pQty) =
        let stillNeeded = cntNeededForPricing - cntTaken
        match stillNeeded = 0<piece> || getCntBuyable (pItem, pQty) = 0<piece> with
        | true -> (cntTaken, rest')
        | false ->
            let cntToTake = min pQty stillNeeded
            let replacer (it', qty') = 
                match it' = pItem with
                | true -> pItem, pQty - cntToTake 
                | false -> it', qty'
            cntTaken + cntToTake, List.map replacer rest'

    let cndQualifiedItems = List.sumBy getCntBuyable itemsUnpaid
    match cndQualifiedItems < cntNeededForPricing with
    | true -> priceState
    | false -> 
        let rest = itemsUnpaid 
                    |> List.fold takeOneLot (0<piece>, itemsUnpaid)
                    |> snd
                        
        applyAnyOfPricingOnce pricing { Rest = rest; TotalPrice = total + price }

let applyPricingOnce (pricing: Pricing) (priceState: PriceState) =
    match pricing with
    | SomeOf sop -> applySomeOfPricingOnce sop priceState
    | AnyOf aop -> applyAnyOfPricingOnce aop priceState
            
let calc pricings itemCodes =
    let items = 
        itemCodes
        |> List.map Item
        |> List.groupBy id
        |> List.map (fun (item, itemGroup) -> item, (List.length itemGroup) * 1<piece>)
    
    pricings 
    |> List.fold (fun st p -> applyPricingOnce p st) { Rest = items; TotalPrice = 0<cent> }
    |> (fun st -> st.TotalPrice)