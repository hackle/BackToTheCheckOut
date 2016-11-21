module MainTests

open Main
open Xunit

[<Fact>]
let ``One single A costs its unit price`` () =    
    let price = [ 'A' ] |> calc
    Assert.Equal(price, 50)

[<Fact>]
let ``3 As cost 130`` () =
    let price = [ 'A'; 'A'; 'A' ] |> calc
    Assert.Equal(price, 130)