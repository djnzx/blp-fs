module MyLib.Tests.TypeEval

open MyLib.Core
open Xunit

type TX = TypeExpr
type TV = TypeValue
type PT = PrimitiveType

let NO_SENSE = System.NotSupportedException()

let exclude(xs: List<'A>, x: 'A) =
    xs |> List.filter ((<>) x)

let rotate(xs: List<'A>) =
    match xs with
    | [] -> []
    | x::xs -> xs @ [x]

let map_primitive(tx: TypeExpr) =
    match tx with
    | TX.Primitive(x) -> x |> TV.Primitive
    | _ -> raise NO_SENSE
    
let keys (m: Map<string, TypeExpr>) =
    m
    |> Map.keys
    |> Seq.toList
    |> List.map (fun x0 -> { Name = x0 } |> TV.Var)
    |> TV.Tuple

let rec typeEval (t: TypeExpr) : TypeValue =
    match t with
    | TX.Primitive(pt) -> pt |> TV.Primitive
    | TX.Var(tv) -> tv |> TV.Var
    | TX.Lookup(ti) -> ti |> TV.Lookup
    | TX.Lambda(tp, tx) -> (tp , tx) |> TV.Lambda
    | TX.Arrow(a, b) -> (a |> typeEval, b |> typeEval) |> TV.Arrow
    | TX.Record(txs) -> txs |> Map.map (fun _k a -> a |> typeEval) |> TV.Record
    | TX.Tuple(txs) -> txs |> List.map typeEval |> TV.Tuple
    | TX.Union(txs) -> txs |> Map.map (fun _k a -> typeEval a) |> TV.Union
    | TX.Sum(txs) -> txs |> List.map typeEval |> TV.Sum
    | TX.List(tx) -> tx |> typeEval |> TV.List
    | TX.Set(tx) -> tx |> typeEval |> TV.Set
    | TX.Map(k, v) -> (k |> typeEval, v |> typeEval) |> TV.Map

    // strictly saying, there is not enough context to implement next 5
    // we need more semantics to do that,
    // but let's speculate:
    | TX.Apply(TX.Arrow(a, b), x) when a = x -> b |> typeEval
    | TX.Apply _ -> raise NO_SENSE

    // keys ? then only to Map<string, TypeExpr>
    | TX.KeyOf(TX.Record(txs))
    | TX.KeyOf(TX.Union(txs)) -> txs |> keys
    | TX.KeyOf _ -> raise NO_SENSE

    // make union of two any types
    | TX.Flatten({ Left = lb; Right = rb }) ->
        match (lb.Type, rb.Type) with
        | TX.Union(l), TX.Union(r) ->
            // should we care about conflicting keys?
            let merge = Map.fold (fun acc k v -> Map.add k v acc)
            merge l r |> TX.Union |> typeEval
        | _ ->
            // probably we need to ensure these are primitives or products
            Map.ofList [
                (lb.Identifier.Name, lb.Type);
                (rb.Identifier.Name, rb.Type)
            ]
            |> TX.Union |> typeEval
    
    // the only implementation I could think of ...
    | TX.Exclude(TX.Sum(txs), x) -> exclude(txs, x) |> TX.Sum |> typeEval
    | TX.Exclude _ -> raise NO_SENSE
    
    // the only implementation I could think of ...
    // we can't do Record, Union since Map is sorted in F# 
    | TX.Rotate(TX.Tuple(ts)) -> ts |> rotate |> TX.Tuple |> typeEval
    // technically we could do the Sum, but there is no sense since the Sum is unordered
    | TX.Rotate(TX.Sum(ts)) -> ts |> rotate |> TX.Sum |> typeEval
    | TX.Rotate _ -> raise NO_SENSE

[<Fact>]
let ``primitive`` () =
    Assert.Equal(
        TypeExpr.Primitive(PrimitiveType.Unit) |> typeEval,
        TypeValue.Primitive(PrimitiveType.Unit)
    )

[<Fact>]
let ``exclude TX -> TV`` () =
    let txs = [TX.Primitive(PrimitiveType.Int); TX.Primitive(PrimitiveType.String); TX.Primitive(PrimitiveType.Unit)]
    let sum = TX.Sum(txs)
    let excludedTV = TX.Exclude(sum, TX.Primitive(PrimitiveType.String)) |> typeEval
    
    let tvs = [TV.Primitive(PrimitiveType.Int); TV.Primitive(PrimitiveType.Unit)]
    let expectedTV = TV.Sum(tvs)
    
    Assert.Equal(excludedTV, expectedTV)

[<Fact>]
let ``rotate - generic`` () =
    let a = [1;2;3]
    let rotated = rotate(a)
    let expected = [2;3;1]
    
    Assert.Equal<List<int>>(rotated, expected)

[<Fact>]
let ``rotate TX -> TV`` () =
    let txs = [TX.Primitive(PrimitiveType.Int); TX.Primitive(PrimitiveType.String); TX.Primitive(PrimitiveType.Unit)]
    let txs_rotated = txs |> rotate
    let tuple = TX.Tuple(txs)
    let tupleR = TX.Rotate(tuple)
    
    let tvs_rotated = txs_rotated |> List.map map_primitive 
    let tupleR2 = TV.Tuple(tvs_rotated)
    
    Assert.Equal(
        typeEval tupleR,
        tupleR2
        )

[<Fact>]
let ``keys - base function`` () =
    let m = 
        Map.ofList [
        ("a", TX.Primitive(PT.String))
        ("b", TX.Primitive(PT.Int))
        ("c", TX.Primitive(PT.Bool))
    ]
    
    Assert.Equal(
        m |> keys,
        TV.Tuple([
            TV.Var({Name = "a"});
            TV.Var({Name = "b"});
            TV.Var({Name = "c"});
        ])
        )

