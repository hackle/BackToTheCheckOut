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

let calcOne { Item = Item code; Quantity = Quantity qty; Price = Price p } (accu: PriceState) =
    let { Items = items; Total = Price total } = accu
    let rest = items |> takeItems code qty 

    match rest with
    | None -> accu
    | Some r -> { Items = r; Total = Price (total + p) }
    

let calc items =
    items
    |> List.sumBy findPrice