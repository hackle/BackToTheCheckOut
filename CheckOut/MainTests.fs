module MainTests

open Main
open Xunit

[<Fact>]
let ``One single A costs its unit price`` () =    
    let price = [ { SKU = 'A'; UnitPrice = 0.50M } ] |> calc
    Assert.Equal(price, 0.50M)