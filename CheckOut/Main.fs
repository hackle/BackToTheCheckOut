module Main

type Item = Item of char
type Price = Price of int with
    static member (+) ((Price p1), (Price p2)) =  Price (p1 + p2)

[<Measure>]
type pc

type SomeOfPricing = { Items: (Item * int<pc>) list; Price: Price }
type AnyOfPricing = { ChooseFrom: Item list; Quantity: int<pc>; Price: Price }
type Pricing = SomeOf of SomeOfPricing | AnyOf of AnyOfPricing
type PriceState = { Items: (Item * int<pc>) list; Total: Price }

type List<'a> with
    static member replaceAt index newItem (lst: (int * 'a) list) =
        lst |> List.map (fun (i, v) -> if i = index then (i, newItem) else (i, v))

let pricings: Pricing list = [
    SomeOf { Items = [ (Item 'A', 3<pc>) ]; Price = Price 130 }
    SomeOf { Items = [ (Item 'A', 1<pc>); (Item 'B', 1<pc>) ]; Price = Price 70 }
    SomeOf { Items = [ (Item 'A', 1<pc>) ]; Price = Price 50 }
    SomeOf { Items = [ (Item 'B', 1<pc>) ]; Price = Price 30 }
    AnyOf { ChooseFrom = [ Item 'C'; Item 'D'; Item 'E' ]; Quantity = 2<pc>; Price = Price 10 }
    SomeOf { Items = [ (Item 'C', 1<pc>) ]; Price = Price 5 }
    SomeOf { Items = [ (Item 'D', 1<pc>) ]; Price = Price 6 }
    SomeOf { Items = [ (Item 'E', 1<pc>) ]; Price = Price 7 }
]

let rec applySomeOfPricingOnce (pricing: SomeOfPricing) (priceState: PriceState) = 
    let { Items = itemsToBuy; Total = total } = priceState

    let pricingCanApply = 
        let canApply' (pIt, pQty) =
            itemsToBuy |> List.exists (fun (bIt, bQty) -> bIt = pIt && bQty >= pQty)
            
        pricing.Items
        |> List.forall canApply'

    if not pricingCanApply
        then priceState
        else 
            let pricingMap = pricing.Items |> dict
            let rest =
                itemsToBuy
                |> List.map (fun (aIt, aQty) -> 
                                if pricingMap.ContainsKey aIt
                                    then aIt, (aQty - pricingMap.Item(aIt))
                                    else (aIt, aQty))
            applySomeOfPricingOnce pricing { Items = rest; Total = total + pricing.Price }

let rec applyAnyOfPricingOnce (pricing: AnyOfPricing) (priceState: PriceState) =
    let { ChooseFrom = itemsToChooseFrom; Quantity = totalForPricing; Price = price } = pricing
    let { Items = itemsToBuy; Total = total } = priceState

    let cntAvailableToTake (item, qty) =
       if List.exists (fun it -> it = item) itemsToChooseFrom
        then qty
        else 0<pc>

    let rec takeTill' (cntTaken, itemsLeft) (index, (item, qty)) =
        let stillNeeded = totalForPricing - cntTaken
        if stillNeeded = 0<pc> || cntAvailableToTake (item, qty) = 0<pc>
            then (cntTaken, itemsLeft)
            else
                let cntToTake = min qty stillNeeded
                let afterTaking = item, qty - cntToTake
                let stillLeft = itemsLeft |> List.replaceAt index afterTaking
                cntTaken + cntToTake, stillLeft

    let cndQualifiedItems = itemsToBuy |> List.sumBy cntAvailableToTake
    if cndQualifiedItems < totalForPricing
        then priceState
        else
            let indexed = itemsToBuy |> List.indexed
            let rest = List.fold takeTill' (0<pc>, indexed) indexed |> snd |> List.map snd
            applyAnyOfPricingOnce pricing { Items = rest; Total = total + price }

let applyPricingOnce (pricing: Pricing) (priceState: PriceState) =
    match pricing with
    | SomeOf sop -> applySomeOfPricingOnce sop priceState
    | AnyOf aop -> applyAnyOfPricingOnce aop priceState
            
let calc itemCodes =
    let items = 
        itemCodes
        |> List.map Item
        |> List.groupBy id
        |> List.map (fun (it, ls) -> it, (List.length ls) * 1<pc>)
    let initialState = { Items = items; Total = Price 0 }

    let finalState =
        pricings
        |> List.fold (fun st p -> applyPricingOnce p st) initialState

    finalState.Total
    