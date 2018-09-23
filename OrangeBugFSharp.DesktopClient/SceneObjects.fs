namespace OrangeBug.DesktopClient

open Layman
open OrangeBug
open OrangeBug.Game
open OrangeBug.DesktopClient
open OrangeBug.DesktopClient.LibUI
open Microsoft.Xna.Framework.Graphics

type TransformComponent = {
    mutable localMatrix: XnaMatrix
    mutable worldMatrix: XnaMatrix
}

module TransformComponent =
    let create() = { localMatrix = XnaMatrix.Identity; worldMatrix = XnaMatrix.Identity }

    let createAt translation = {
        localMatrix = XnaMatrix.CreateTranslation(translation)
        worldMatrix = XnaMatrix.Identity
    }

    let updateWorldMatrices scene =
        let update parentWorldMatrix _ transform =
            transform.worldMatrix <- transform.localMatrix * parentWorldMatrix
            transform.worldMatrix
        scene |> SceneGraph.iterComponents update XnaMatrix.Identity


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
        
        scene.tree |> Tree.fold drawNode stateToResult mergeCommands []


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
                TransformComponent.createAt (XnaVector3(float32 p.x, float32 p.y, 0.0f))
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
                    XnaMatrix.CreateRotationZ (orientation comp.tile).AsRadians *
                    XnaMatrix.CreateTranslation (comp.position.AsVector3 0.0f)
            | _ -> ()
        scene |> SceneGraph.iterComponents updateNode ()


type EntityComponent = {
    mutable entity: Entity
    mutable positionAnimation: XnaVector3 Animation
}

module EntityComponent =
    let nodeId (id: EntityId) = sprintf "Entity(%i)" id.id

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
                    XnaMatrix.CreateRotationZ (orientation comp.entity).AsRadians *
                    XnaMatrix.CreateTranslation translation

            match node |> SceneNode.tryGetComponent<SpriteRendererComponent> with
            | None -> ()
            | Some renderer ->
                let sprite = "Sprites/" + Rendering.entitySpriteKey comp.entity |> getSprite
                renderer.sprite <- Some sprite

        scene |> SceneGraph.iterComponents updateNode ()


type CameraComponent = {
    /// World-to-view matrix
    mutable viewMatrix: XnaMatrix
    /// View-to-projection matrix
    mutable projectionMatrix: XnaMatrix
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
                { size = size; viewMatrix = XnaMatrix.Identity; projectionMatrix = XnaMatrix.Identity; }
            ]
        }
    
    let updateCameraMatrices (scene: SceneGraph) aspectRatio =
        let updateNode _ node camera =
            match node |> SceneNode.tryGetComponent<TransformComponent> with
            | None -> ()
            | Some transform ->
                camera.viewMatrix <- XnaMatrix.Invert transform.worldMatrix
                camera.projectionMatrix <- XnaMatrix.CreateOrthographic(camera.size * aspectRatio, camera.size, -1000.0f, 1000.0f);
        
        scene |> SceneGraph.iterComponents updateNode ()


type UIComponent = {
    ui: UI
}

module UIComponent =

    let dummyInvalidate _ _ = ()

    let updateUI (viewport: Viewport) (comp: UIComponent) =
        // Re-render dirty elements
        UI.update comp.ui

        // Perform layout pass. If nothing changed, LayoutCaches let this terminate quickly.
        let root = comp.ui.root
        let layout = root.behavior.layout (elementInfo dummyInvalidate root)
        let space = SysVector2(float32 viewport.Width, float32 viewport.Height)
        let context = { parentCache = None; traceWriter = ignore }
        let _, measureData = Layout.measure space context layout
        Layout.arrange (rect SysVector2.Zero space) measureData context layout |> ignore

    let updateUIs scene viewport =
        scene |> SceneGraph.iterComponents (fun _ _ -> updateUI viewport) ()

    let renderUI = function
        | Leave(context, _) -> context
        | Enter(context, node) ->
            match node |> SceneNode.tryGetComponent<UIComponent> with
            | Some ui -> ui.ui.root.behavior.draw (elementInfo dummyInvalidate ui.ui.root) context 
            | _ -> context

    let renderUIs scene (graphicsDevice: GraphicsDevice) =
        let context = VisualLayer.beginFrame graphicsDevice
        scene.tree
        |> Tree.foldEvents renderUI context
        |> VisualLayer.endFrame
