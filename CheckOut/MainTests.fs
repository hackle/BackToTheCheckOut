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

// C D E priced 5 6 7 
[<Theory>]
[<InlineData(1, 0, 0, 5)>]
[<InlineData(0, 1, 0, 6)>]
[<InlineData(0, 0, 1, 7)>]
[<InlineData(1, 1, 0, 10)>]
[<InlineData(0, 1, 1, 10)>]
[<InlineData(1, 0, 1, 10)>]
[<InlineData(2, 0, 0, 10)>]
[<InlineData(0, 0, 2, 10)>]
[<InlineData(0, 2, 0, 10)>]
[<InlineData(4, 2, 0, 30)>]
[<InlineData(5, 2, 0, 36)>]
[<InlineData(5, 1, 0, 30)>]
let ``Price for any 2 of 'C', 'D' and 'E'`` (cntC, cntD, cntE, expectedPrice) =
    let c's = 'C' |> List.replicate cntC
    let d's = 'D' |> List.replicate cntD
    let e's = 'E' |> List.replicate cntE

    let (Price price) = c's @ d's @ e's |> calc
    Assert.Equal(price, expectedPrice)
