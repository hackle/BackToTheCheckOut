module MainTests

open Domain
open Main2
open Xunit

let getPricings(): Pricing list = [
    SomeOf { Items = [ (Item 'A', 3<piece>) ]; Price = 130<cent> }
    SomeOf { Items = [ (Item 'A', 1<piece>); (Item 'B', 1<piece>) ]; Price = 70<cent> }
    SomeOf { Items = [ (Item 'A', 1<piece>) ]; Price = 50<cent> }
    SomeOf { Items = [ (Item 'B', 1<piece>) ]; Price = 30<cent> }
    AnyOf { ChooseFrom = [ Item 'C'; Item 'D'; Item 'E' ]; Quantity = 2<piece>; Price = 10<cent> }
    SomeOf { Items = [ (Item 'C', 1<piece>) ]; Price = 5<cent> }
    SomeOf { Items = [ (Item 'D', 1<piece>) ]; Price = 6<cent> }
    SomeOf { Items = [ (Item 'E', 1<piece>) ]; Price = 7<cent> }
]

[<Theory>]
[<InlineData(1, 50<cent>)>]
[<InlineData(2, 100<cent>)>]
[<InlineData(3, 130<cent>)>]
[<InlineData(4, 180<cent>)>]
[<InlineData(6, 260<cent>)>]
let ``Price for only 'A's`` (cnt, expectedPrice) =    
    let price = 'A' |> List.replicate cnt |> calc (getPricings())
    Assert.Equal(price, expectedPrice)

[<Theory>]
[<InlineData(1, 0, 50<cent>)>]
[<InlineData(0, 1, 30<cent>)>]
[<InlineData(1, 1, 70<cent>)>]
[<InlineData(2, 1, 120<cent>)>]
[<InlineData(1, 2, 100<cent>)>]
let ``Price for combo of 'A' and 'B'`` (cntA, cntB, expectedPrice) =
    let a's = 'A' |> List.replicate cntA
    let b's = 'B' |> List.replicate cntB

    let price = a's @ b's |> calc (getPricings())
    Assert.Equal(price, expectedPrice)

// C D E priced 5 6 7 
[<Theory>]
[<InlineData(1, 0, 0, 5<cent>)>]
[<InlineData(0, 1, 0, 6<cent>)>]
[<InlineData(0, 0, 1, 7<cent>)>]
[<InlineData(1, 1, 0, 10<cent>)>]
[<InlineData(0, 1, 1, 10<cent>)>]
[<InlineData(1, 0, 1, 10<cent>)>]
[<InlineData(2, 0, 0, 10<cent>)>]
[<InlineData(0, 0, 2, 10<cent>)>]
[<InlineData(0, 2, 0, 10<cent>)>]
[<InlineData(4, 2, 0, 30<cent>)>]
[<InlineData(5, 2, 0, 36<cent>)>]
[<InlineData(5, 1, 0, 30<cent>)>]
let ``Price for any 2 of 'C', 'D' and 'E'`` (cntC, cntD, cntE, expectedPrice) =
    let c's = 'C' |> List.replicate cntC
    let d's = 'D' |> List.replicate cntD
    let e's = 'E' |> List.replicate cntE

    let price = c's @ d's @ e's |> calc (getPricings())
    Assert.Equal(price, expectedPrice)
