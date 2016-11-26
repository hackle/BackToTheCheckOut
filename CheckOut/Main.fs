module Main

type Item = Item of char

[<Measure>]
type pc

[<Measure>]
type cent

type SomeOfPricing = { Items: (Item * int<pc>) list; Price: int<cent> }
type AnyOfPricing = { ChooseFrom: Item list; Quantity: int<pc>; Price: int<cent> }
type Pricing = SomeOf of SomeOfPricing | AnyOf of AnyOfPricing
type PriceState = { Items: (Item * int<pc>) list; Total: int<cent> }

type List<'a> with
    static member replaceAt index newItem (lst: (int * 'a) list) =
        lst |> List.map (fun (i, v) -> if i = index then (i, newItem) else (i, v))

let pricings: Pricing list = [
    SomeOf { Items = [ (Item 'A', 3<pc>) ]; Price = 130<cent> }
    SomeOf { Items = [ (Item 'A', 1<pc>); (Item 'B', 1<pc>) ]; Price = 70<cent> }
    SomeOf { Items = [ (Item 'A', 1<pc>) ]; Price = 50<cent> }
    SomeOf { Items = [ (Item 'B', 1<pc>) ]; Price = 30<cent> }
    AnyOf { ChooseFrom = [ Item 'C'; Item 'D'; Item 'E' ]; Quantity = 2<pc>; Price = 10<cent> }
    SomeOf { Items = [ (Item 'C', 1<pc>) ]; Price = 5<cent> }
    SomeOf { Items = [ (Item 'D', 1<pc>) ]; Price = 6<cent> }
    SomeOf { Items = [ (Item 'E', 1<pc>) ]; Price = 7<cent> }
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
    let initialState = { Items = items; Total = 0<cent> }

    let finalState =
        pricings
        |> List.fold (fun st p -> applyPricingOnce p st) initialState

    finalState.Total
    