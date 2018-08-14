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

    let update scene =
        let updateNode id node parentWorldMatrix =
            let m = node.localMatrix * parentWorldMatrix
            { node with worldMatrix = m }, m
        scene |> SceneGraph.iterAndUpdate updateNode Matrix.Identity

open TransformNode

module SpriteRendererNode =
    type SpriteRendererNodeState = {
        sprite: Sprite option
    }

    let draw scene =
        let mutable commands = []

        let drawNode id (node: obj) (worldMatrix: Matrix) =
            match node with
            | :? TransformNode.TransformNodeState as node -> node.worldMatrix
            | :? SpriteRendererNodeState as node ->
                match node.sprite with
                | Some sprite -> commands <- (DrawSprite (sprite, worldMatrix)) :: commands
                | None -> ()
                worldMatrix
            | _ -> worldMatrix
        
        scene |> SceneGraph.iter drawNode Matrix.Identity
        commands

open SpriteRendererNode

module TileRendererNode =
    type TileRendererNodeState = {
        tile: Tile
        //orientation: Direction
    }

    let createSubtree p tile =
        let transformNode = { id = sprintf "Tile(%i, %i)" p.x p.y; state = TransformNode.create (Vector3(float32 p.x, float32 p.y, 0.0f)) }
        let tileNode = { id = sprintf "Tile(%i, %i)/Tile" p.x p.y; state = { tile = tile } }
        let rendererNode = { id = sprintf "Tile(%i, %i)/Tile/Renderer" p.x p.y; state = { sprite = None }}
        TreeNode (transformNode, [ TreeNode (tileNode, [ TreeNode (rendererNode, []) ]) ])

    let spriteKey =
        function
        | PathTile -> "Path"
        | WallTile -> "Wall"
        | LockedTile -> "" // shouldn't happen
        | InkTile state -> "Ink" + (match state.color with Red -> "Red" | Green -> "Green" | Blue -> "Blue")
        | PinTile state -> "Pin" + (match state.color with Red -> "Red" | Green -> "Green" | Blue -> "Blue")
        | ButtonTile _ -> "Button"
        | GateTile state -> if state.isOpen then "GateOpened" else "GateClosed"
        | TeleporterTile _ -> "Teleporter"
        | CornerTile _ -> "Corner"
        | PistonTile _ -> "Piston"

    let update scene getSprite =
        let updateNode id (node: obj) sprite =
            match node with
            | :? TileRendererNodeState as node ->
                let sprite = "Sprites/" + spriteKey node.tile |> getSprite
                node :> obj, Some sprite
            | :? SpriteRendererNodeState as node ->
                { node with sprite = sprite } :> obj, sprite
            | other -> other, sprite
        
        scene |> SceneGraph.iterAndUpdate updateNode None

module EntityRendererNode =
    type EntityRendererNodeState = {
        entity: Entity
        position: Point
        //orientation: Direction
    }

    let createSubtree id p entity =
        let entityNode = { id = sprintf "Entity(%i)" id.id; state = { entity = entity; position = p } }
        let transformNode = { id = sprintf "Entity(%i)/Transform" id.id; state = TransformNode.create Vector3.Zero }
        let rendererNode = { id = sprintf "Entity(%i)/Transform/Renderer" id.id; state = { sprite = None }}
        TreeNode (entityNode, [ TreeNode (transformNode, [ TreeNode (rendererNode, []) ]) ])

    let spriteKey =
        function
        | PlayerEntity _ -> "Player"
        | BoxEntity _ -> "Box"
        | BalloonEntity state -> "Balloon" + (match state.color with Red -> "Red" | Green -> "Green" | Blue -> "Blue")
        | PistonEntity _ -> "PistonEntity"

    let update scene getSprite =
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
                    { node with sprite = Some sprite } :> obj, None
                | None -> node :> obj, None
            | other -> other, e
        
        scene |> SceneGraph.iterAndUpdate updateNode None


module CameraNode =
    type CameraNodeState = {
        /// World-to-view matrix
        viewMatrix: Matrix
        /// View-to-projection matrix
        projectionMatrix: Matrix
        /// Size of the vertical viewing volume (horizontal size calculated accordingly)
        size: float32
    }

    // View matrix:
    //     "remapping the World Space so that the camera is in the origin and looks down along the Z axis"
    //     it's the "inverse" of the camera's Model-to-World matrix
    //     (but can be computed more cleverly than taking the inverse)
    //
    // Projection matrix:
    //     "projects onto the image"
    //     geometry outside [-1,1]-cuboid is clipped
    //     here: project directly into viewRect?
    //
    // GPU then drops Z-component and maps [-1,1] to [0,imageWidth/Height]
    // Putting it together: Model-To-World + World-To-View + View-To-Projection (+ GPU-stuff)

    let createSubtree id size position =
        let transformNode = { id = id; state = TransformNode.create position }
        let cameraNode = { id = id + "/Camera"; state = { viewMatrix = Matrix.Identity; projectionMatrix = Matrix.Identity; size = size } }
        TreeNode (transformNode, [ TreeNode (cameraNode, []) ])

    let update (scene: SceneGraph) aspectRatio =
        let updateNode id (node: obj) worldMatrix =
            match node with
            | :? TransformNodeState as node -> node :> obj, node.worldMatrix
            | :? CameraNodeState as node ->
                let newState = {
                    viewMatrix = Matrix.Invert worldMatrix
                    projectionMatrix = Matrix.CreateOrthographic(node.size * aspectRatio, node.size, -1000.0f, 1000.0f);
                    size = node.size
                }
                newState :> obj, worldMatrix
            | _ as node -> node, worldMatrix
        
        scene |> SceneGraph.iterAndUpdate updateNode Matrix.Identity