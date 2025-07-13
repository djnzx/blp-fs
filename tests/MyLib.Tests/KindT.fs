namespace KindT

open Xunit
open Xunit.Abstractions
open MyLib.Core

type KindRelated(output: ITestOutputHelper) =

    let rec kindToString k =
        match k with
        | Star -> "*"
        | Kind.Arrow(Star, Star) -> "* -> *"
        | Kind.Arrow(Star, k2) -> $"* -> ({(kindToString k2)})"
        | Kind.Arrow(k1, Star) -> $"({kindToString k1}) -> *"
        | Kind.Arrow(k1, k2) -> $"({kindToString k1}) -> ({kindToString k2})"

    [<Fact>]
    let ``kind to string`` () =
        let k1 = Star // *
        let k2 = Kind.Arrow(k1, k1)
        let k3a = Kind.Arrow(k2, k1)
        let k3b = Kind.Arrow(k1, k2)

        Assert.Equal(k1 |> kindToString, "*")
        Assert.Equal(k2 |> kindToString, "* -> *")
        Assert.Equal(k3a |> kindToString, "(* -> *) -> *")
        Assert.Equal(k3b |> kindToString, "* -> (* -> *)")
