namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open OrangeBug
open OrangeBug.Game

type TransformComponent = {
    mutable localMatrix: Matrix
    mutable worldMatrix: Matrix
}

module TransformComponent =
    let create() = { localMatrix = Matrix.Identity; worldMatrix = Matrix.Identity }

    let createAt translation = {
        localMatrix = Matrix.CreateTranslation(translation)
        worldMatrix = Matrix.Identity
    }

    let updateWorldMatrices scene =
        let update parentWorldMatrix _ transform =
            transform.worldMatrix <- transform.localMatrix * parentWorldMatrix
            transform.worldMatrix
        scene |> SceneGraph.iterComponents update Matrix.Identity


type SpriteRendererComponent = {
    mutable sprite: Sprite option
}

module SpriteRendererComponent =
    let create() = { sprite = None }

    let draw scene =
        let drawNode commands node =
            match node |> SceneNode.tryGetComponent<SpriteRendererComponent>, node |> SceneNode.tryGetComponent<TransformComponent> with
            | Some renderer, Some transform ->
                match renderer.sprite with
                | None -> commands
                | Some sprite -> DrawSprite (sprite, transform.worldMatrix) :: commands
            | _ -> commands

        let stateToResult commands = commands
        let mergeCommands lists = List.concat lists
        
        scene |> SceneGraph.fold drawNode stateToResult mergeCommands []


type TileComponent = {
    position: Point
    mutable tile: Tile
}

module TileComponent =
    let nodeId p = sprintf "Tile(%i, %i)" p.x p.y

    let createNode p tile =
        {
            id = nodeId p
            components = [
                TransformComponent.createAt (Vector3(float32 p.x, float32 p.y, 0.0f))
                SpriteRendererComponent.create()
                { position = p; tile = tile }
            ]
        }

    let orientation =
        function
        | CornerTile state -> state.orientation
        | PistonTile state -> state.orientation
        | _ -> North

    let update scene getSprite =
        let updateNode _ node comp =
            match node |> SceneNode.tryGetComponent<SpriteRendererComponent>, node |> SceneNode.tryGetComponent<TransformComponent> with
            | Some renderer, Some transform ->
                renderer.sprite <- "Sprites/" + Rendering.tileSpriteKey comp.tile |> getSprite |> Some
                transform.localMatrix <-
                    Matrix.CreateRotationZ (orientation comp.tile).AsRadians *
                    Matrix.CreateTranslation (comp.position.AsVector3 0.0f)
            | _ -> ()
        scene |> SceneGraph.iterComponents updateNode ()

type 'a Animation = {
    source: 'a
    target: 'a
    startTime: SimTime
    duration: SimTimeSpan
}

module Ease =
    let linear t = t
    let quadraticIn t = t * t
    let quadraticOut t = -t * (t - 2)
    let quadraticInOut t =
        if t < 0.5f
        then 0.5f * t * t
        else let t = t - 1.0f in -0.5f * (t * (t - 2.0f) - 1.0f)
    let sineIn t = 1.0f - (cos (t * pi'2))
    let sineOut t = sin (t * pi'2)
    let sineInOut t = -0.5f * (cos (t * pi) - 1.0f)

module Animation =
    let create source target startTime duration =
        { source = source; target = target; startTime = startTime; duration = duration }

    let constant value = create value value (SimTime 0) (SimTimeSpan 0)

    let evaluate interpolate (ease: float32 -> float32) simTime animation =
        let elapsed = simTime - float32 animation.startTime.value
        let progress = clamp01 (elapsed / float32 animation.duration.value) |> ifNaN 0.0f |> ease
        interpolate animation.source animation.target progress

    let evaluateFloat = evaluate lerp
    let evaluateVector2 = evaluate (fun a b t -> Vector2.Lerp(a, b, t))
    let evaluateVector3 = evaluate (fun a b t -> Vector3.Lerp(a, b, t))
    let evaluateVector4 = evaluate (fun a b t -> Vector4.Lerp(a, b, t))
    let evaluateMatrix = evaluate (fun a b t -> Matrix.Lerp(a, b, t))

type EntityComponent = {
    mutable entity: Entity
    mutable positionAnimation: Vector3 Animation
}

module EntityComponent =
    let nodeId id = sprintf "Entity(%i)" id.id

    let createNode id (p: Point) entity =
        {
            id = nodeId id
            components = [
                TransformComponent.create()
                SpriteRendererComponent.create()
                { entity = entity; positionAnimation = Animation.constant (p.AsVector3 1.0f) ; }
            ]
        }

    let orientation =
        function
        | PlayerEntity state -> state.orientation
        | PistonEntity state -> state.orientation
        | _ -> North

    let update scene getSprite simTime =
        let updateNode _ node comp =            
            match node |> SceneNode.tryGetComponent<TransformComponent> with
            | None -> ()
            | Some transform ->
                let translation = comp.positionAnimation |> Animation.evaluateVector3 Ease.sineOut simTime
                transform.localMatrix <-
                    Matrix.CreateRotationZ (orientation comp.entity).AsRadians *
                    Matrix.CreateTranslation translation

            match node |> SceneNode.tryGetComponent<SpriteRendererComponent> with
            | None -> ()
            | Some renderer ->
                let sprite = "Sprites/" + Rendering.entitySpriteKey comp.entity |> getSprite
                renderer.sprite <- Some sprite

        scene |> SceneGraph.iterComponents updateNode ()


type CameraComponent = {
    /// World-to-view matrix
    mutable viewMatrix: Matrix
    /// View-to-projection matrix
    mutable projectionMatrix: Matrix
    /// Size of the vertical viewing volume (horizontal size calculated accordingly)
    mutable size: float32
}

module CameraComponent =
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

    let createNode id size position =
        {
            id = id
            components = [
                TransformComponent.createAt position
                { size = size; viewMatrix = Matrix.Identity; projectionMatrix = Matrix.Identity; }
            ]
        }
    
    let updateCameraMatrices (scene: SceneGraph) aspectRatio =
        let updateNode _ node camera =
            match node |> SceneNode.tryGetComponent<TransformComponent> with
            | None -> ()
            | Some transform ->
                camera.viewMatrix <- Matrix.Invert transform.worldMatrix
                camera.projectionMatrix <- Matrix.CreateOrthographic(camera.size * aspectRatio, camera.size, -1000.0f, 1000.0f);
        
        scene |> SceneGraph.iterComponents updateNode ()