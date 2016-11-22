module Main


type Item = Item of char
type Price = Price of int
type Quantity = Quantity of int
type Pricing = { Item: Item; Quantity: Quantity; Price: Price }
type PriceState = { Items: (Item * Quantity) list; Total: Price }

let pricings = [
    { Item = Item 'A'; Quantity = Quantity 3; Price = Price 130 }
    { Item = Item 'A'; Quantity = Quantity 1; Price = Price 50 }
]

let takeItems code qty (items: (Item * Quantity) list) =
    let found = items |> List.tryFind (fun (Item it, Quantity q) -> it = code && q >= qty)
    match found with
    | None -> None
    | Some foundItem -> 
        let (Item it, Quantity q) = foundItem
        let rest = items |> List.map (fun it' -> 
                                        if it' = foundItem 
                                        then (Item it, Quantity (q - qty)) 
                                        else it')
        Some rest

let rec calcOne pricing (accu: PriceState) =
    let { Item = Item code; Quantity = Quantity qty; Price = Price p } = pricing
    let { Items = items; Total = Price total } = accu
    let rest = items |> takeItems code qty 

    match rest with
    | None -> accu
    | Some r -> calcOne pricing { Items = r; Total = Price (total + p) }
    

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
    