module Main

type Item = Item of char
type Price = Price of int
type Quantity = Quantity of int
type SomeOfPricing = { Items: (Item * Quantity) list; Price: Price }
type AnyOfPricing = { ChooseFrom: Item list; Any: Quantity; Price: Price }
type Pricing = SomeOfPricing | AnyOfPricing
type PriceState = { Items: (Item * Quantity) list; Total: Price }

let (-) (Quantity qty1) (Quantity qty2) =
    Quantity (qty1 - qty2)

let (+) (Price p1) (Price p2) =
    Price (p1 + p2)

let pricings = [
    { Items = [ (Item 'A', Quantity 3) ]; Price = Price 130 }
    { Items = [ (Item 'A', Quantity 1); (Item 'B', Quantity 1) ]; Price = Price 70 }
    { Items = [ (Item 'A', Quantity 1) ]; Price = Price 50 }
    { Items = [ (Item 'B', Quantity 1) ]; Price = Price 30 }
]

let rec applyPricingOnce (pricing: Pricing) (priceState: PriceState) = 
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
            applyPricingOnce pricing { Items = rest; Total = total + pricing.Price }
            
let calc itemCodes =
    let items = 
        itemCodes
        |> List.map Item
        |> List.groupBy id
        |> List.map (fun (it, ls) -> (it, Quantity (ls |> List.length)))
    let initialState = { Items = items; Total = Price 0 }

    let finalState =
        pricings
        |> List.fold (fun st p -> applyPricingOnce p st) initialState

    finalState.Total
    