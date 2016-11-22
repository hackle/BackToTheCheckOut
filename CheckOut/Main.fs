module Main


type Item = Item of char
type Price = Price of int
type Quantity = Quantity of int
type Pricing = { Items: (Item * Quantity) list; Price: Price }
type PriceState = { Items: (Item * Quantity) list; Total: Price }

let pricings = [
    { Items = [ (Item 'A', Quantity 3) ]; Price = Price 130 }
    { Items = [ (Item 'A', Quantity 1); (Item 'B', Quantity 1) ]; Price = Price 70 }
    { Items = [ (Item 'A', Quantity 1) ]; Price = Price 50 }
    { Items = [ (Item 'B', Quantity 1) ]; Price = Price 30 }
]

let tryApply (pricing: Pricing) (items: (Item * Quantity) list) =
    let take' (Item item, Quantity qty) =
        let itemInPricing = pricing.Items |> List.tryFind (fun (Item it, _) -> it = item)
        match itemInPricing with
        | None -> None
        | Some (_, Quantity qw) -> 
            let q' = qty - qw
            if q' < 0
                then None
                else Some (Item item, Quantity q')

    let attempt = items |> List.map take'
    if attempt |> List.exists Option.isNone
        then None
        else attempt |> List.map (fun v -> v.Value) |> Some

let rec calcOne pricing (accu: PriceState) =
    let { Price = Price price } = pricing
    let { Items = items; Total = Price total } = accu
    let rest = items |> tryApply pricing

    match rest with
    | None -> accu
    | Some r -> calcOne pricing { Items = r; Total = Price (total + price) }    

let calc itemCodes =
    let items = 
        itemCodes
        |> List.map Item
        |> List.groupBy id
        |> List.map (fun (it, ls) -> (it, Quantity (ls |> List.length)))
    let initialState = { Items = items; Total = Price 0 }

    let finalState =
        pricings
        |> List.fold (fun st p -> calcOne p st) initialState

    finalState.Total
    