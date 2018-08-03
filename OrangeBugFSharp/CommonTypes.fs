namespace OrangeBug

open Newtonsoft.Json
open Microsoft.FSharp.Reflection

[<AutoOpen>]
module UnionHelper =
    /// <summary>
    /// Unwraps a union value into a specific union case.
    /// If the union value does not match that case, returns None.
    /// Caution: Union cases with identical field types are considered equivalent,
    /// e.g. having "type U = A of int | B of int", "unwrap A (B 5) = Some 5".
    /// </summary>
    let unwrapAs (ctor: 'a -> 'u) (v: 'u) : 'a option =
        let case, fields = FSharpValue.GetUnionFields(v, typedefof<'u>)
        let caseTypes = case.GetFields() |> Array.map (fun f -> f.PropertyType)
        let desiredType, _ = FSharpType.GetFunctionElements(ctor.GetType())

        if FSharpType.IsTuple desiredType then
            let neededTypes = FSharpType.GetTupleElements desiredType
            if caseTypes = neededTypes
            then Some (FSharpValue.MakeTuple(fields, desiredType) :?> 'a)
            else None
        else
            if caseTypes = [| desiredType |]
            then Some (fields.[0] :?> 'a)
            else None

[<Struct>]
type Point = { x: int; y : int }
type InkColor = Red | Green | Blue
type Direction = North | East | South | West

type GameTimeSpan = 
    | GameTimeSpan of int
    member this.value = match this with GameTimeSpan v -> v
    static member (+) (a: GameTimeSpan, b: GameTimeSpan) = GameTimeSpan (a.value + b.value)

type GameTime =
    | GameTime of int // 4 ticks per second
    member this.value = match this with GameTime v -> v
    static member (+) (time: GameTime, duration: GameTimeSpan) = GameTime (time.value + duration.value)

[<AutoOpen>]
module Point =
    let (|Point|) p = (p.x, p.y)

type Point with
    static member create x y = { x = x; y = y }
    static member zero = Point.create 0 0
    static member one = Point.create 1 1
    static member unitX = Point.create 1 0
    static member unitY = Point.create 0 1
    static member (+) (p, q) = Point.create (p.x + q.x) (p.y + q.y)
    static member (-) (p, q) = Point.create (p.x - q.x) (p.y - q.y)
    static member (~-) p = Point.create -p.x -p.y
    static member (*) (c, p) = Point.create (c * p.x) (c * p.y)

    [<JsonIgnore>]
    member this.asDirection =
        match (this.x, this.y) with
        | (0, 1) -> Some North
        | (1, 0) -> Some East
        | (0, -1) -> Some South
        | (-1, 0) -> Some West
        | _ -> None

type Direction with
    static member tryParse (s: string) =
        match s.ToLower() with
        | "north" -> Some North
        | "east" -> Some East
        | "south" -> Some South
        | "west" -> Some West
        | _ -> None
    member this.asPoint =
        match this with
        | North -> Point.unitY
        | East -> Point.unitX
        | South -> -Point.unitY
        | West -> -Point.unitX
