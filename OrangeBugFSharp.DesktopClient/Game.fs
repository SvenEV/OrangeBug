﻿namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open OrangeBug
open OrangeBug.Game
open OrangeBug.Hosting

type Game() as g =
    inherit Microsoft.Xna.Framework.Game()

    do g.Content.RootDirectory <- @"D:\Projects\Experimental\OrangeBugFSharp\OrangeBugFSharp.DesktopClient\bin\DesktopGL\Assets" //"../../Assets" // TODO: This doesn't work
    do g.IsMouseVisible <- true
    do g.Window.AllowUserResizing <- true
    let graphics = new GraphicsDeviceManager(g, PreferMultiSampling = true, HardwareModeSwitch = true)
    let mutable scene = SceneGraph.empty
    let mutable mapSize = Point.zero

    let getSprite name =
        let texture = g.Content.Load<Texture2D> name
        { texture = texture; size = Vector2.One; anchorPoint = 100.0f * Vector2.One }

    let handleEvent ev =
        let updateTileComponent p updater =
            match scene |> SceneGraph.tryGetComponent<TileComponent> (TileComponent.nodeId p) with
            | Some comp -> updater comp
            | None -> ()
        let updateEntityComponent id updater =
            match scene |> SceneGraph.tryGetComponent<EntityComponent> (EntityComponent.nodeId id) with
            | Some comp -> updater comp
            | None -> ()

        match ev.event with
        | PlayerRotatedEvent ev -> updateEntityComponent ev.entityId (fun c -> c.entity <- PlayerEntity ev.player)
        | EntityMovedEvent ev -> updateEntityComponent ev.entityId (fun c -> c.position <- ev.newPosition)
        | BalloonPoppedEvent ev -> scene <- scene |> SceneGraph.remove (EntityComponent.nodeId ev.entityId)
        | BalloonColoredEvent ev ->
            updateEntityComponent ev.entityId (fun c -> c.entity <- BalloonEntity ev.balloon)
            updateTileComponent ev.inkPosition (fun c -> c.tile <- PathTile)
        | _ -> ()

    let onSignal =
        function
        | ReceiveInitialMap (map, time, tickTargetTime) ->
            let camera = CameraComponent.createNode "MainCamera" 10.0f (Vector3((map.size.x / 2 |> float32) - 0.5f, (map.size.y / 2 |> float32) - 0.5f, 10.0f))

            let tiles =
                map.tiles
                |> Grid.asSeq
                |> Seq.map (fun (p, t) -> TileComponent.createNode p t.tile)

            let entities =
                map.entities
                |> Seq.map (fun kvp -> EntityComponent.createNode kvp.Key kvp.Value.position kvp.Value.entity)

            scene <-
                Seq.singleton camera
                |> Seq.append tiles
                |> Seq.append entities
                |> Seq.fold (fun g n -> SceneGraph.addOrReplace SceneGraph.rootId n g) SceneGraph.empty
            
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

        match scene |> SceneGraph.tryGetComponent<CameraComponent> "MainCamera" with
        | None -> ()
        | Some cam ->
            cam.size <-
                if aspectRatioScreen > aspectRatioMap
                then float32 mapSize.y
                else (float32 mapSize.x) / aspectRatioScreen
        
        TransformComponent.updateWorldMatrices scene
        CameraComponent.updateCameraMatrices scene aspectRatioScreen
        TileComponent.update scene getSprite
        EntityComponent.update scene getSprite
        base.Update(gameTime)

    override g.Draw gameTime =
        g.GraphicsDevice.Clear Color.DarkBlue

        match scene |> SceneGraph.tryGetComponent<CameraComponent> "MainCamera" with
        | None -> ()
        | Some cam ->
            SpriteRendererComponent.draw scene
            |> SpriteBatch3D.draw g.GraphicsDevice cam.viewMatrix cam.projectionMatrix

        base.Draw(gameTime)