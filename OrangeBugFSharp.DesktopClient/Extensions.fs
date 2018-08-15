[<AutoOpen>]
module OrangeBug.DesktopClient.Extensions

open Microsoft.Xna.Framework
open OrangeBug

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
        | West -> MathHelper.PiOver2
        | South -> MathHelper.Pi
        | East -> MathHelper.Pi + MathHelper.PiOver2