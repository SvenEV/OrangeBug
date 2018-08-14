namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open OrangeBug
open OrangeBug.Game
open OrangeBug.Hosting
open EntityRendererNode
open CameraNode

type Game() as g =
    inherit Microsoft.Xna.Framework.Game()

    do g.Content.RootDirectory <- @"D:\Projects\Experimental\OrangeBugFSharp\OrangeBugFSharp.DesktopClient\bin\DesktopGL\Assets" //"../../Assets" // TODO: This doesn't work
    do g.IsMouseVisible <- true
    do g.Window.AllowUserResizing <- true
    let graphics = new GraphicsDeviceManager(g, PreferMultiSampling = true, HardwareModeSwitch = true)
    let mutable scene = EmptyTree
    let mutable mapSize = Point.zero

    let getSprite name =
        let texture = g.Content.Load<Texture2D> name
        { texture = texture; size = Vector2.One; anchorPoint = 0.5f * Vector2.One }

    let handleEvent ev =
        match ev.event with
        | EntityMovedEvent ev ->
            let nodeId = sprintf "Entity(%i)" ev.entityId.id
            scene |> SceneGraph.update<EntityRendererNodeState> nodeId (fun state -> { state with position = ev.newPosition } :> obj)
        |_ -> ()

    let onSignal =
        function
        | ReceiveInitialMap (map, time, tickTargetTime) ->
            let tiles =
                map.tiles
                |> Grid.asSeq
                |> Seq.map (fun (p, t) -> TileRendererNode.createSubtree p t.tile)

            let entities =
                map.entities
                |> Seq.map (fun kvp -> EntityRendererNode.createSubtree kvp.Key kvp.Value.position kvp.Value.entity)
            
            let camera = CameraNode.createSubtree "MainCamera" 10.0f (Vector3(map.size.x / 2 |> float32, map.size.y / 2 - 1 |> float32, 10.0f))
            let all = tiles |> Seq.append entities |> Seq.append (Seq.singleton camera)
            scene <- TreeNode (SceneGraph.rootNode, all |> List.ofSeq)
            mapSize <- map.size

        | ReceiveEvents (events, time) ->
            events |> Seq.iter handleEvent
        | _ -> ()


    override g.Initialize() =
        SessionManager.create onSignal (Simulation.create <| SampleMaps.createInitialMap()) "game"
        base.Initialize()

    override g.LoadContent() = ()

    override g.Update gameTime =
        if Keyboard.GetState().IsKeyDown(Keys.Escape) then g.Exit()

        let requestMove dir = SessionManager.handleRequest "game" (RequestIntent (MovePlayerIntent { name = "Player"; direction = dir }))
        if Keyboard.GetState().IsKeyDown(Keys.Right) then requestMove East
        if Keyboard.GetState().IsKeyDown(Keys.Left) then requestMove West
        if Keyboard.GetState().IsKeyDown(Keys.Up) then requestMove North
        if Keyboard.GetState().IsKeyDown(Keys.Down) then requestMove South

        let aspectRatioScreen = (float32 g.GraphicsDevice.Viewport.Width) / float32 g.GraphicsDevice.Viewport.Height
        let aspectRatioMap = (float32 mapSize.x) / float32 mapSize.y

        scene |> SceneGraph.update<CameraNodeState> "MainCamera/Camera" (fun cam ->
            let size =
                if aspectRatioScreen > aspectRatioMap
                then float32 mapSize.y
                else (float32 mapSize.x) / aspectRatioScreen
            { cam with size = size } :> obj)
        
        TransformNode.update scene
        CameraNode.update scene aspectRatioScreen
        TileRendererNode.update scene getSprite
        EntityRendererNode.update scene getSprite
        base.Update(gameTime)

    override g.Draw gameTime =
        g.GraphicsDevice.Clear Color.DarkBlue

        let camera = scene |> SceneGraph.getAs<CameraNodeState> "MainCamera/Camera"

        SpriteRendererNode.draw scene
        |> SpriteBatch3D.draw g.GraphicsDevice camera.viewMatrix camera.projectionMatrix

        base.Draw(gameTime)