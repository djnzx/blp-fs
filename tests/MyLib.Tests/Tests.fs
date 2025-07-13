module MyLib.Tests.Tests

open MyLib.Functions.Functions
open Xunit
open Xunit.Abstractions

type Tests(output: ITestOutputHelper) =

    [<Fact>]
    let ``primitive types`` () =
        // SIGNED
        // sbyte
        // int16
        // int
        // int64
        //
        // UNSIGNED
        // byte
        // -
        // uint32
        // uint64
        //
        // float = double
        // float32
        //
        // char
        // string
        //
        // bool
        let a = 255uy
        let b: double = 32768
        let u = ()
        let x1 = 3
        let x2: int = 3
        Assert.Equal(x1, x2)

    [<Fact>]
    let ``implicit / explicit type`` () =
        let x1 = 3
        let x2: int = 3
        Assert.Equal(x1, x2)

    [<Fact>]
    let ``should add two numbers`` () =
        let x = 3
        let y = 4
        let z = add (x, y)
        Assert.Equal(7, z)

    [<Fact>]
    let ``tuple syntax`` () =
        let tuple = (1, "alex")
        // explicit type
        let tuple2: int * string * float = (1, "alex", 3.5)
        let (id, name) = tuple
        Assert.Equal(7, 7)

    [<Fact>]
    let ``func call / composition`` () =
        let twice (x: int) = x * 2

        let k1 = twice 10
        Assert.Equal(k1, 20)

        let k2 = 10 |> twice
        Assert.Equal(k2, 20)

        let t2 = twice >> twice
        let k3 = 10 |> t2
        Assert.Equal(k3, 40)

    [<Fact>]
    let ``curry`` () =
        let add a b = a + b // int by default
        let add (a: float) (b: float) = a + b

        let add10a a = add 10 a
        let r1 = add10a 25

        let add10b = add 10
        let r2 = add10b 25

        let r3 = add 3.51 3.53

        Assert.Equal(r1, 35)
        Assert.Equal(r2, 35)
        Assert.Equal(r3, 7.04, 3)

    [<Fact>]
    let ``collections - list`` () =
        let l1 = [ 1; 2; 3 ]
        let l2 = 0 :: l1
        Assert.Equal<int list>(l2, [ 0; 1; 2; 3 ])

        let h :: t = l2 // could fail
        Assert.Equal(h, 0)
        Assert.Equal<int list>([ 1; 2; 3 ], t)

    [<Fact>]
    let ``collections - array`` () =
        let a = [| 10; 20; 30 |]
        Assert.Equal(a[0], 10)
        a[0] <- 100
        Assert.Equal(a[0], 100)

    [<Fact>]
    let ``pattern match`` () =
        let l = [ 1; 2; 3 ]

        let mks l =
            match l with
            | [] -> $"List is empty"
            | h :: t -> $"head: {h}, tail: {t}"

        let s1 = mks []
        let s2 = mks l

        output.WriteLine(s1)
        output.WriteLine(s2)

        Assert.Equal(s1, "List is empty")
        Assert.Equal(s2, "head: 1, tail: [2; 3]")

    [<Fact>]
    let ``collections - seq`` () =
        let sq = seq { 3..5 }
        let expected = [| 3; 4; 5 |]
        let sq2 = Seq.toArray sq
        Assert.Equal(expected, sq)

    [<Fact>]
    let ``collections - set`` () =
        let set1 = Set.ofList [ 1; 2; 2; 3 ]
        let set2 = Set.ofList [ 2; 3; 1 ]
        Assert.Equal<Set<int>>(set1, set2)

    [<Fact>]
    let ``collections - other`` () =
        let map = Map.ofList [ ("a", 1); ("b", 2) ]

        Assert.True(true)
