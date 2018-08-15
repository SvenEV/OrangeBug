namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open OrangeBug
open System
open System

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