module Tests

open MyLib.Functions.Functions
open Xunit

[<Fact>]
let ``should add two numbers`` () =
    let x = 3
    let y = 4
    let z = add (x, y)
    
    Assert.Equal(7, z)
