module MainTests

open Main
open Xunit

[<Theory>]
[<InlineData(1, 50)>]
[<InlineData(2, 100)>]
[<InlineData(3, 130)>]
[<InlineData(4, 180)>]
[<InlineData(6, 260)>]
let ``Price for only 'A's`` (cnt, expectedPrice) =    
    let (Price price) = 'A' |> List.replicate cnt |> calc
    Assert.Equal(price, expectedPrice)

[<Theory>]
[<InlineData(1, 0, 50)>]
[<InlineData(0, 1, 30)>]
[<InlineData(1, 1, 70)>]
[<InlineData(2, 1, 120)>]
[<InlineData(1, 2, 100)>]
let ``Price for combo of 'A' and 'B'`` (cntA, cntB, expectedPrice) =
    let a's = 'A' |> List.replicate cntA
    let b's = 'B' |> List.replicate cntB

    let (Price price) = a's @ b's |> calc
    Assert.Equal(price, expectedPrice)