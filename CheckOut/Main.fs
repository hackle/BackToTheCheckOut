module Main

type Item = Item of char

[<Measure>]
type piece

[<Measure>]
type cent

type SomeOfPricing = { Items: (Item * int<piece>) list; Price: int<cent> }
type AnyOfPricing = { ChooseFrom: Item list; Quantity: int<piece>; Price: int<cent> }
type Pricing = SomeOf of SomeOfPricing | AnyOf of AnyOfPricing
type PriceState = { Rest: (Item * int<piece>) list; TotalPrice: int<cent> }

type List<'a> with
    static member replaceAt index newItem (ls: (int * 'a) list) =
        ls |> List.map (fun (i, v) -> if i = index then (i, newItem) else (i, v))

let rec applySomeOfPricingOnce (pricing: SomeOfPricing) (priceState: PriceState) = 
    let { Rest = rest; TotalPrice = totalPrice } = priceState
    
    let isBuyingEnoughForPricing (pItem, pQuantity) =
        let isEnoughAndMatching (bItem, bQuantity) = bItem = pItem && bQuantity >= pQuantity
        List.exists isEnoughAndMatching rest

    let pricingCanApply = List.forall isBuyingEnoughForPricing pricing.Items

    if not pricingCanApply
        then priceState
        else 
            let pricingMap = pricing.Items |> dict
            let takeApplicableItems (itemLeft, quantityLeft) =
                if pricingMap.ContainsKey itemLeft
                    then itemLeft, (quantityLeft - pricingMap.Item(itemLeft))
                    else (itemLeft, quantityLeft)

            let rest = List.map takeApplicableItems rest
            applySomeOfPricingOnce pricing { Rest = rest; TotalPrice = totalPrice + pricing.Price }

let rec applyAnyOfPricingOnce (pricing: AnyOfPricing) (priceState: PriceState) =
    let { ChooseFrom = itemsToChooseFrom; Quantity = cntNeededForPricing; Price = price } = pricing
    let { Rest = itemsUnpaid; TotalPrice = total } = priceState

    let getCntBuyable (bItem, bQuantity) =
       if List.exists ((=) bItem) itemsToChooseFrom then bQuantity else 0<piece>

    let rec takeOneLot (cntTaken, indexedItemsUnpaid) (index, (pItem, pQty)) =
        let stillNeeded = cntNeededForPricing - cntTaken
        if stillNeeded = 0<piece> || getCntBuyable (pItem, pQty) = 0<piece>
            then (cntTaken, indexedItemsUnpaid)
            else
                let cntToTake = min pQty stillNeeded
                let afterTaking = pItem, pQty - cntToTake
                let stillLeft = List.replaceAt index afterTaking indexedItemsUnpaid
                cntTaken + cntToTake, stillLeft

    let cndQualifiedItems = List.sumBy getCntBuyable itemsUnpaid
    if cndQualifiedItems < cntNeededForPricing
        then priceState
        else
            let indexed = List.indexed itemsUnpaid
            let rest = indexed 
                        |> List.fold takeOneLot (0<piece>, indexed) 
                        |> snd
                        |> List.map snd
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
    let initialState = { Rest = items; TotalPrice = 0<cent> }

    let finalState = List.fold (fun st p -> applyPricingOnce p st) initialState pricings

    finalState.TotalPrice