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