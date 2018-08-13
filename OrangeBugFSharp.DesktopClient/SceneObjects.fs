namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open OrangeBug.Game
open Microsoft.Xna.Framework.Graphics

module TransformNode =
    type TransformNodeState = {
        localMatrix: Matrix
        worldMatrix: Matrix
    }

    let update (scene: Scene) =
        let updateNode id node parentWorldMatrix =
            let m = node.localMatrix * parentWorldMatrix
            scene.Update id { node with worldMatrix = m }
            m
        scene.IterPreorder updateNode Matrix.Identity Scene.rootId

module TileRendererNode =
    type TileRendererNodeState = {
        tile: Tile
    }

    let spriteKey =
        function
        | PathTile -> "Path"
        | WallTile -> "Wall"
        | LockedTile -> "" // shouldn't happen
        | InkTile state -> sprintf "Ink%A" state.color
        | PinTile state -> sprintf "Pin%A" state.color
        | ButtonTile state -> "Button"
        | GateTile state -> if state.isOpen then "GateOpened" else "GateClosed"
        | TeleporterTile state -> "Teleporter"
        | CornerTile state -> "Corner"
        | PistonTile state -> "Piston"

    let draw (scene: Scene) (getSprite: string -> Texture2D) (spriteBatch: SpriteBatch) =
        let drawNode id (node: obj) (position: Vector2) =
            match node with
            | :? TransformNode.TransformNodeState as node -> node.worldMatrix.Translation.XY
            | :? TileRendererNodeState as node ->
                let sprite = spriteKey node.tile |> sprintf "Sprites/%s" |> getSprite
                spriteBatch.Draw(sprite, position, Color.White)
                position
            | _ -> position
        scene.IterPreorder drawNode Vector2.Zero Scene.rootId