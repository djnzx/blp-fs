module MyLib.Tests.Unions

open Xunit
open Xunit.Abstractions

type Rgb1 = { R: int; G: int; B: int }
type Rgb2 = { R: int; G: int; B: int }

type Color =
    | Red
    | Yellow of double
    | Green of int
    | RGB of int * int * int
    | RGB2 of Rgb2

type Unions(output: ITestOutputHelper) =

    [<Fact>]
    let ``explore syntax`` () =
        let c1 = Red
        let c2 = Yellow 3.5
        let c3 = Green 33
        let c5 = RGB(1, 2, 3)
        let color1: Rgb1 = { R = 255; G = 128; B = 64 }
        let color2: Rgb2 = { R = 255; G = 128; B = 64 }
        let c6 = RGB2 color2

        Assert.True(true)
