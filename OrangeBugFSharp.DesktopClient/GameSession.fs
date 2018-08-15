namespace OrangeBug.DesktopClient

open System
open OrangeBug
open OrangeBug.Game
open OrangeBug.Hosting
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type GameSession = {
    id: string
    mutable map: GameMapState
    mutable scene: SceneGraph
    mutable queuedSignals: Signal list
    mutable lastTimeSync: TimeSpan * SimTime
    mutable tickTargetTime: float32
}

module GameSession =
    let handleEvent session ev =
        let updateTileComponent p updater =
            match session.scene |> SceneGraph.tryGetComponent<TileComponent> (TileComponent.nodeId p) with
            | Some comp -> updater comp
            | None -> ()
        let updateEntityComponent id updater =
            match session.scene |> SceneGraph.tryGetComponent<EntityComponent> (EntityComponent.nodeId id) with
            | Some comp -> updater comp
            | None -> ()

        match ev.event with
        | PlayerRotatedEvent e -> updateEntityComponent e.entityId (fun c -> c.entity <- PlayerEntity e.player)
        | EntityMovedEvent e -> updateEntityComponent e.entityId (fun c -> c.positionAnimation <- Animation.create (e.oldPosition.AsVector3 1.0f) (e.newPosition.AsVector3 1.0f) ev.time ev.duration)
        | BalloonPoppedEvent e -> session.scene <- session.scene |> SceneGraph.remove (EntityComponent.nodeId e.entityId)
        | BalloonColoredEvent e ->
            updateEntityComponent e.entityId (fun c -> c.entity <- BalloonEntity e.balloon)
            updateTileComponent e.inkPosition (fun c -> c.tile <- PathTile)
        | _ -> ()

    let handleSignal session (gameTime: GameTime) =
        function
        | ReceiveEvents (events, time) ->
            session.lastTimeSync <- gameTime.TotalGameTime, time
            events |> Seq.iter (handleEvent session)
        | _ -> ()

    let buildScene map =
        let cameraPos = Vector3((map.size.x / 2 |> float32) - 0.5f, (map.size.y / 2 |> float32) - 0.5f, 10.0f)
        let camera = CameraComponent.createNode "MainCamera" 10.0f cameraPos

        let tiles =
            map.tiles
            |> Grid.asSeq
            |> Seq.map (fun (p, t) -> TileComponent.createNode p t.tile)

        let entities =
            map.entities
            |> Seq.map (fun kvp -> EntityComponent.createNode kvp.Key kvp.Value.position kvp.Value.entity)

        Seq.singleton camera
        |> Seq.append tiles
        |> Seq.append entities
        |> Seq.fold (fun g n -> SceneGraph.addOrReplace SceneGraph.rootId n g) SceneGraph.empty

    let create id =
        let session = { id = id; map = GameMap.empty; scene = SceneGraph.empty; queuedSignals = []; lastTimeSync = TimeSpan.Zero, SimTime 0; tickTargetTime = 0.0f }
        let onSignal signal = session.queuedSignals <- signal :: session.queuedSignals
        let map, time, tickTargetTime = SessionManager.create onSignal (Simulation.create SampleMaps.sampleMap1) id
        session.map <- map
        session.scene <- buildScene map
        session.lastTimeSync <- TimeSpan.Zero, time
        session.tickTargetTime <- float32 tickTargetTime
        session

    let gameToSimTime session (gameTime: GameTime) =
        let syncGameTime, syncSimTime = session.lastTimeSync
        float32 syncSimTime.value + (float32 <| gameTime.TotalGameTime.TotalSeconds - syncGameTime.TotalSeconds) / session.tickTargetTime
    
    let update session getSprite aspectRatioScreen gameTime =
        let requestMove dir = SessionManager.handleRequest session.id (RequestIntent (MovePlayerIntent { name = "Player"; direction = dir }))
        if Keyboard.GetState().IsKeyDown(Keys.Right) then requestMove East
        if Keyboard.GetState().IsKeyDown(Keys.Left) then requestMove West
        if Keyboard.GetState().IsKeyDown(Keys.Up) then requestMove North
        if Keyboard.GetState().IsKeyDown(Keys.Down) then requestMove South

        // handle queued signals
        session.queuedSignals |> List.iter (handleSignal session gameTime)
        session.queuedSignals <- []

        let simTime = gameToSimTime session gameTime

        // update camera size so map fits onto screen
        match session.scene |> SceneGraph.tryGetComponent<CameraComponent> "MainCamera" with
        | None -> ()
        | Some cam ->
            cam.size <-
                let aspectRatioMap = (float32 session.map.size.x) / float32 session.map.size.y
                if aspectRatioScreen > aspectRatioMap
                    then float32 session.map.size.y
                    else (float32 session.map.size.x) / aspectRatioScreen
        
        // update scene
        TransformComponent.updateWorldMatrices session.scene
        CameraComponent.updateCameraMatrices session.scene aspectRatioScreen
        TileComponent.update session.scene getSprite
        EntityComponent.update session.scene getSprite simTime

    let draw session graphicsDevice =
        match session.scene |> SceneGraph.tryGetComponent<CameraComponent> "MainCamera" with
        | None -> ()
        | Some cam ->
            SpriteRendererComponent.draw session.scene
            |> SpriteBatch3D.draw graphicsDevice cam.viewMatrix cam.projectionMatrix