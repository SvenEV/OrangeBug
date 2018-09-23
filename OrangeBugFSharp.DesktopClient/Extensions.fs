namespace OrangeBug.DesktopClient

open OrangeBug

[<AutoOpen>]
module Extensions =
    type Point with
        member p.AsVector2 = XnaVector2(float32 p.x, float32 p.y)
        member p.AsVector3 z = XnaVector3(float32 p.x, float32 p.y, z)

    type Direction with
        member d.AsRadians =
            match d with
            | North -> 0.0f
            | West -> pi'2
            | South -> pi
            | East -> pi + pi'2
