namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open OrangeBug
open System
open System.Runtime.CompilerServices
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq

[<AutoOpen>]
module Mathf =
    let sin (t: float32) = t |> float |> Math.Sin |> float32
    let cos (t: float32) = t |> float |> Math.Cos |> float32
    let clamp (v: float32) min max = MathHelper.Clamp(v, min , max)
    let clamp01 v = clamp v 0.0f 1.0f
    let lerp (a: float32) b progress = MathHelper.Lerp(a, b, progress)
    let min (a: float32) b = MathHelper.Min(a, b)
    let max (a: float32) b = MathHelper.Max(a, b)
    let pi = MathHelper.Pi
    let pi'2 = MathHelper.PiOver2
    let pi'4 = MathHelper.PiOver4
    let vec2 x y = Vector2(x, y)
    let vec3 x y z = Vector3(x, y, z)
    let ifNaN fallback v = if Single.IsNaN v then fallback else v

[<AutoOpen>]
module Extensions =
    let (|KeyValuePair|) (kvp: KeyValuePair<'key, 't>) = kvp.Key, kvp.Value
    
    [<Extension>]
    type LaymanExtensions =
        [<Extension>]
        static member inline AsXna(v: Layman.Vector2) = Vector2(float32 v.x, float32 v.y)

    type Vector3 with
        member v.XY = Vector2(v.X, v.Y)
        member v.XZ = Vector2(v.X, v.Y)

    type Point with
        member p.AsVector2 = Vector2(float32 p.x, float32 p.y)
        member p.AsVector3 z = Vector3(float32 p.x, float32 p.y, z)

    type Direction with
        member d.AsRadians =
            match d with
            | North -> 0.0f
            | West -> pi'2
            | South -> pi
            | East -> pi + pi'2

module KeyValuePair =
    let asTuple (kvp: KeyValuePair<'key, 't>) = kvp.Key, kvp.Value
    let ofTuple (k, v) = KeyValuePair(k, v)
    let key (kvp: KeyValuePair<'key, 't>) = kvp.Key
    let value (kvp: KeyValuePair<'key, 't>) = kvp.Value

module Dictionary =
    let tryFind key (dict: IDictionary<'key, 't>) =
        match dict.TryGetValue key with
        | true, v -> Some v
        | _ -> None
    
    let ofSeq = ImmutableDictionary.CreateRange
    let ofTupleSeq items = items |> Seq.map KeyValuePair.ofTuple |> ofSeq

    let iter f (dict: IDictionary<'key, 't>) =
        dict :> IEnumerable<KeyValuePair<'key, 't>>
        |> Seq.iter (fun kvp -> f kvp.Key kvp.Value)

    let equal (a: IDictionary<'key, 't>) (b: IDictionary<'key, 't>) =
        a.Count = b.Count && Enumerable.SequenceEqual(a, b) // note: count check for performance only

[<AutoOpen>]
module DictionaryExtensions =
    type IDictionary<'key, 't> with
        member dict.TryFind key = Dictionary.tryFind key dict

module ImmutableHashSet =
    let ofSeq = ImmutableHashSet.CreateRange
    let union (a: ImmutableHashSet<'a>) b = a.Union b

module Seq =
    let fullOuterJoin keyA keyB projection a b =
        let alookup = a |> Seq.map (fun v -> keyA v, v) |> dict // assumption: no duplicate keys
        let blookup = b |> Seq.map (fun v -> keyB v, v) |> dict
        ImmutableHashSet.union (ImmutableHashSet.ofSeq alookup.Keys) (ImmutableHashSet.ofSeq blookup.Keys)
        |> Seq.map (fun key ->
            let xa = alookup.TryFind key
            let xb = blookup.TryFind key
            projection xa xb key)
