[<AutoOpen>]
module OrangeBug.DesktopClient.Extensions

open Microsoft.Xna.Framework

type Vector3 with
    member v.XY = Vector2(v.X, v.Y)
    member v.XZ = Vector2(v.X, v.Y)