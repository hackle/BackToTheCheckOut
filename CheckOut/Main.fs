module Main

type Item = { SKU: char; UnitPrice: decimal }

let calc items =
    items
    |> List.sumBy (fun i -> i.UnitPrice)