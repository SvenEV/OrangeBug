namespace OrangeBug.DesktopClient

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq
open System.Runtime.CompilerServices
open Microsoft.Xna.Framework.Graphics

type SysVector2 = System.Numerics.Vector2
type SysVector3 = System.Numerics.Vector3
type SysVector4 = System.Numerics.Vector4
type SysMatrix = System.Numerics.Matrix4x4

type XnaVector2 = Microsoft.Xna.Framework.Vector2
type XnaVector3 = Microsoft.Xna.Framework.Vector3
type XnaVector4 = Microsoft.Xna.Framework.Vector4
type XnaMatrix = Microsoft.Xna.Framework.Matrix
type XnaColor = Microsoft.Xna.Framework.Color
type XnaMath = Microsoft.Xna.Framework.MathHelper

[<AutoOpen>]
module Mathf =
    let sin (t: float32) = t |> float |> Math.Sin |> float32
    let cos (t: float32) = t |> float |> Math.Cos |> float32
    let clamp (v: float32) min max = XnaMath.Clamp(v, min , max)
    let clamp01 v = clamp v 0.0f 1.0f
    let lerp (a: float32) b progress = XnaMath.Lerp(a, b, progress)
    let pi = XnaMath.Pi
    let pi'2 = XnaMath.PiOver2
    let pi'4 = XnaMath.PiOver4
    let ifNaN fallback v = if Single.IsNaN v then fallback else v

[<AutoOpen>]
module Extensions =
    let (|KeyValuePair|) (kvp: KeyValuePair<'key, 't>) = kvp.Key, kvp.Value
    
    [<Extension>]
    type VectorExtensions =
        [<Extension>]
        static member inline AsXna(v: SysVector2) = XnaVector2(v.X, v.Y)
        [<Extension>]
        static member inline AsSys(v: XnaVector2) = SysVector2(v.X, v.Y)
    
    type System.Numerics.Vector3 with
        member v.XY = SysVector2(v.X, v.Y)
        member v.XZ = SysVector2(v.X, v.Y)

    type Microsoft.Xna.Framework.Vector3 with
        member v.XY = XnaVector2(v.X, v.Y)
        member v.XZ = XnaVector2(v.X, v.Y)

    type Microsoft.Xna.Framework.Rectangle with
        member r.TopLeft = XnaVector2(float32 r.X, float32 r.Y)
        member r.BottomRight = XnaVector2(float32 r.Right, float32 r.Bottom)
        member r.SizeF = XnaVector2(float32 r.Width, float32 r.Height)

    type Texture2D with
        member t.Size = XnaVector2(float32 t.Width, float32 t.Height)

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

type Quad = {
    vertices: VertexPositionColorTexture[]
}

module Quad =
    let indices = [| 0; 1; 2; 2; 1; 3 |]

    let to3d (v: XnaVector2) = XnaVector3(v.X, v.Y, 0.0f)

    let create (position: XnaVector2) (size: XnaVector2) color = {
        vertices = [|
            // To account for the flipped Y axis (y=0 is top), the UVs are flipped here (textures would be upside-down otherwise)
            VertexPositionColorTexture(to3d position, color, XnaVector2.Zero) // bottom left
            VertexPositionColorTexture(to3d (position + XnaVector2.UnitY * size.Y), color, XnaVector2.UnitY) // top left
            VertexPositionColorTexture(to3d (position + XnaVector2.UnitX * size.X), color, XnaVector2.UnitX) // bottom right
            VertexPositionColorTexture(to3d (position + size), color, XnaVector2.One) // top right
        |]
    }

    let createUV (position: XnaVector2) (size: XnaVector2) color (uvMin: XnaVector2) (uvMax: XnaVector2) = {
        vertices = [|
            // To account for the flipped Y axis (y=0 is top), the UVs are flipped here (textures would be upside-down otherwise)
            VertexPositionColorTexture(to3d position, color, uvMin) // bottom left
            VertexPositionColorTexture(to3d (position + XnaVector2.UnitY * size.Y), color, XnaVector2(uvMin.X, uvMax.Y)) // top left
            VertexPositionColorTexture(to3d (position + XnaVector2.UnitX * size.X), color, XnaVector2(uvMax.X, uvMin.Y)) // bottom right
            VertexPositionColorTexture(to3d (position + size), color, uvMax) // top right
        |]
    }