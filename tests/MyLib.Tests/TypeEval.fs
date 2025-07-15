module MyLib.Tests.TypeEval

open MyLib.Core
open Xunit
open Xunit.Abstractions

type TypeEval(output: ITestOutputHelper) =

    let y = lazy raise (System.Exception("boom"))

    let rec typeEval (t: TypeExpr) : TypeValue =
        match t with
        | TypeExpr.Primitive(pt) -> TypeValue.Primitive(pt)
        | TypeExpr.Var(tv) -> TypeValue.Var(tv)
        | TypeExpr.Lookup(tv) -> TypeValue.Lookup(tv)
        | TypeExpr.Lambda(tp, tx) -> TypeValue.Lambda(tp, tx)
        | TypeExpr.Arrow(tx1, tx2) -> TypeValue.Arrow(typeEval tx1, typeEval tx2)
        | TypeExpr.Record(m) -> TypeValue.Record(Map.map (fun _k a -> typeEval a) m)
        | TypeExpr.Tuple(txs) -> TypeValue.Tuple(List.map typeEval txs)
        | TypeExpr.Union(m) -> TypeValue.Union(Map.map (fun _k a -> typeEval a) m)
        | TypeExpr.Sum(txs) -> TypeValue.Sum(List.map typeEval txs)
        | TypeExpr.List(tx) -> TypeValue.List(typeEval tx)
        | TypeExpr.Set(tx) -> TypeValue.Set(typeEval tx)
        | TypeExpr.Map(tx1, tx2) -> TypeValue.Map(typeEval tx1, typeEval tx1)
        // strictly saying, there is not enough context to implement next 5
        // we need more semantics to do that
        | TypeExpr.Apply(tx1, tx2) ->
            // this could relate to Arrow or Lambda
            raise (System.NotImplementedException())    
        | TypeExpr.KeyOf(tx) -> raise (System.NotImplementedException())
        | TypeExpr.Flatten(fa) ->
            let l = fa.Left
            let lidv = l.Identifier |> TypeExpr.Lookup |> typeEval
            let ltxv = l.Type |> typeEval
            let r = fa.Right
            let ridv = r.Identifier |> TypeExpr.Lookup |> typeEval
            let rtxv = r.Type |> typeEval
            raise (System.NotImplementedException())
        | TypeExpr.Exclude(tx1, tx2) -> raise (System.NotImplementedException())
        | TypeExpr.Rotate(tx) -> raise (System.NotImplementedException())

    [<Fact>]
    let ``primitive`` () =
        Assert.Equal(
            typeEval (TypeExpr.Primitive(PrimitiveType.Unit)),
            TypeValue.Primitive(PrimitiveType.Unit)
            )
