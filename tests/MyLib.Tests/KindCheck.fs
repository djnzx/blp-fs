module MyLib.Tests.KindCheck

open MyLib.Core
open Xunit
open Xunit.Abstractions

type KindCheck(output: ITestOutputHelper) =

    let ensure1st (k: Kind) =
        match k with
        | Star -> Star
        | a -> failwithf $"expected to have *, actual {a}"

    // maybe it's better to have Either[E, A] instead of A + Exception
    let rec kind (t: TypeExpr) : Kind =
        match t with
        | TypeExpr.Primitive _ -> Star
        | TypeExpr.Var _ -> Star
        | TypeExpr.Lookup _ -> Star

        | TypeExpr.List ts
        | TypeExpr.Set ts -> ts |> kind |> ensure1st
        | TypeExpr.Map(k, v) ->
            let _ = k |> kind |> ensure1st
            v |> kind |> ensure1st

        | TypeExpr.Record ts
        | TypeExpr.Union ts ->
            let _ = ts |> Map.map (fun _ t -> t |> kind |> ensure1st)
            Star

        | TypeExpr.Tuple ts
        | TypeExpr.Sum ts ->
            let _ = ts |> List.map (fun t -> t |> kind |> ensure1st)
            Star

        // is arrow a function?
        | TypeExpr.Arrow(a, b) ->
            let _ = a |> kind |> ensure1st
            b |> kind

        // is it a function application?
        | TypeExpr.Apply(f, x) ->
            match kind f with
            | Kind.Arrow(a, b) ->
                let kx = x |> kind
                if kx = a then b
                else failwithf $"Apply: Kind mismatch: expected {a} got {kx}"
            | x -> failwithf $"Apply: expected to be Arrow, got {x}"

        | TypeExpr.Lambda(param, x) ->
            Kind.Arrow(param.Kind, x |> kind)

        | TypeExpr.KeyOf t -> t |> kind |> ensure1st
        
        // unless flatten is for List[List[A]] -> List[A]
        | TypeExpr.Flatten fa ->
            let _ = fa.Left.Type |> kind |> ensure1st
            fa.Right.Type |> kind |> ensure1st

        | TypeExpr.Exclude(tx1, tx2) -> failwith "have no clue..."
        | TypeExpr.Rotate typeExpr -> failwith "have no clue..."

    let rec kindCheck (t: TypeExpr) : bool =
        try
            let _ = kind t
            true
        with _ ->
            false

    [<Fact>]
    let ``primitive`` () = Assert.Equal(1, 1)
