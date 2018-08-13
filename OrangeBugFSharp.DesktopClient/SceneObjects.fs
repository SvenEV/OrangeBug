namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open OrangeBug
open OrangeBug.Game
open Microsoft.Xna.Framework.Graphics
open System

module TransformNode =
    type TransformNodeState = {
        localMatrix: Matrix
        worldMatrix: Matrix
    }

    let create translation = {
        localMatrix = Matrix.CreateTranslation(translation)
        worldMatrix = Matrix.Identity
    }

    let update (scene: SceneGraph) =
        let updateNode id node parentWorldMatrix =
            let m = node.localMatrix * parentWorldMatrix
            { node with worldMatrix = m }, m
        scene |> SceneGraph.iterAndUpdate updateNode Matrix.Identity
open TransformNode

module SpriteRendererNode =
    type SpriteRendererNodeState = {
        sprite: Texture2D
        size: Vector2
    }

    let draw (scene: SceneGraph) (spriteBatch: SpriteBatch) =
        let drawNode id (node: obj) (position: Vector2) =
            match node with
            | :? TransformNode.TransformNodeState as node -> node.worldMatrix.Translation.XY
            | :? SpriteRendererNodeState as node ->
                let rect = Rectangle(int position.X, int position.Y, int node.size.X, int node.size.Y) // TODO: Full matrix transformation (incl. scale, rotation, ...)
                spriteBatch.Draw(node.sprite, rect, Nullable(), Color.White)
                position
            | _ -> position
        scene |> SceneGraph.iter drawNode Vector2.Zero

open SpriteRendererNode

module TileRendererNode =
    type TileRendererNodeState = { tile: Tile }

    let createSubtree p tile =
        let transformNode = { id = sprintf "Tile(%i, %i)" p.x p.y; state = TransformNode.create (Vector3(float32 p.x, float32 p.y, 0.0f)) }
        let tileNode = { id = sprintf "Tile(%i, %i)/Tile" p.x p.y; state = { tile = tile } }
        let rendererNode = { id = sprintf "Tile(%i, %i)/Tile/Renderer" p.x p.y; state = { sprite = null; size = Vector2(1.0f, 1.0f) }}
        TreeNode (transformNode, [ TreeNode (tileNode, [ TreeNode (rendererNode, []) ]) ])

    let spriteKey =
        function
        | PathTile -> "Path"
        | WallTile -> "Wall"
        | LockedTile -> "" // shouldn't happen
        | InkTile state -> "Ink" + (match state.color with Red -> "Red" | Green -> "Green" | Blue -> "Blue")
        | PinTile state -> "Pin" + (match state.color with Red -> "Red" | Green -> "Green" | Blue -> "Blue")
        | ButtonTile state -> "Button"
        | GateTile state -> if state.isOpen then "GateOpened" else "GateClosed"
        | TeleporterTile state -> "Teleporter"
        | CornerTile state -> "Corner"
        | PistonTile state -> "Piston"

    let update (scene: SceneGraph) (getSprite: string -> Texture2D) =
        let updateNode id (node: obj) sprite =
            match node with
            | :? TileRendererNodeState as node ->
                let sprite = "Sprites/" + spriteKey node.tile |> getSprite
                node :> obj, Some sprite
            | :? SpriteRendererNodeState as node ->
                match sprite with
                | Some sprite -> { node with sprite = sprite } :> obj, Some sprite
                | None -> node :> obj, None
            | other -> other, sprite
        
        scene |> SceneGraph.iterAndUpdate updateNode None

module EntityRendererNode =
    type EntityRendererNodeState = {
        entity: Entity
        position: Point
    }

    let createSubtree id p entity =
        let entityNode = { id = sprintf "Entity(%i)" id.id; state = { entity = entity; position = p } }
        let transformNode = { id = sprintf "Entity(%i)/Transform" id.id; state = TransformNode.create Vector3.Zero }
        let rendererNode = { id = sprintf "Entity(%i)/Transform/Renderer" id.id; state = { sprite = null; size = Vector2(1.0f, 1.0f) }}
        TreeNode (entityNode, [ TreeNode (transformNode, [ TreeNode (rendererNode, []) ]) ])

    let spriteKey =
        function
        | PlayerEntity _ -> "Player"
        | BoxEntity _ -> "Box"
        | BalloonEntity state -> "Balloon" + (match state.color with Red -> "Red" | Green -> "Green" | Blue -> "Blue")
        | PistonEntity _ -> "PistonEntity"

    let update (scene: SceneGraph) (getSprite: string -> Texture2D) =
        let updateNode id (node: obj) (e: EntityRendererNodeState option) =
            match node with
            | :? EntityRendererNodeState as node ->
                node :> obj, Some node
            | :? TransformNodeState as node ->
                match e with
                | Some e ->
                    let v = Vector3(float32 e.position.x, float32 e.position.y, 1.0f)
                    { node with localMatrix = Matrix.CreateTranslation(v.X, v.Y, v.Z) } :> obj, Some e
                | None -> node :> obj, None
            | :? SpriteRendererNodeState as node ->
                match e with
                | Some entityNode ->
                    let sprite = "Sprites/" + spriteKey entityNode.entity |> getSprite
                    { node with sprite = sprite } :> obj, None
                | None -> node :> obj, None
            | other -> other, e
        
        scene |> SceneGraph.iterAndUpdate updateNode None