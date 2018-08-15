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
    mutable lastSyncTime: TimeSpan
    mutable simTime: float32
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
        | PlayerRotatedEvent ev -> updateEntityComponent ev.entityId (fun c -> c.entity <- PlayerEntity ev.player)
        | EntityMovedEvent ev -> updateEntityComponent ev.entityId (fun c -> c.position <- ev.newPosition)
        | BalloonPoppedEvent ev -> session.scene <- session.scene |> SceneGraph.remove (EntityComponent.nodeId ev.entityId)
        | BalloonColoredEvent ev ->
            updateEntityComponent ev.entityId (fun c -> c.entity <- BalloonEntity ev.balloon)
            updateTileComponent ev.inkPosition (fun c -> c.tile <- PathTile)
        | _ -> ()

    let onSignal session =
        function
        | ReceiveEvents (events, time) ->
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
        let session = { id = id; map = GameMap.empty; scene = SceneGraph.empty; lastSyncTime = TimeSpan.Zero; simTime = 0.0f; tickTargetTime = 0.0f }
        let map, time, tickTargetTime = SessionManager.create (onSignal session) (Simulation.create SampleMaps.sampleMap1) id
        session.map <- map
        session.scene <- buildScene map
        session.simTime <- float32 time.value
        session.tickTargetTime <- float32 tickTargetTime
        session

    let update session getSprite aspectRatioScreen gameTime =
        let requestMove dir = SessionManager.handleRequest session.id (RequestIntent (MovePlayerIntent { name = "Player"; direction = dir }))
        if Keyboard.GetState().IsKeyDown(Keys.Right) then requestMove East
        if Keyboard.GetState().IsKeyDown(Keys.Left) then requestMove West
        if Keyboard.GetState().IsKeyDown(Keys.Up) then requestMove North
        if Keyboard.GetState().IsKeyDown(Keys.Down) then requestMove South

        match session.scene |> SceneGraph.tryGetComponent<CameraComponent> "MainCamera" with
        | None -> ()
        | Some cam ->
            cam.size <-
                let aspectRatioMap = (float32 session.map.size.x) / float32 session.map.size.y
                if aspectRatioScreen > aspectRatioMap
                    then float32 session.map.size.y
                    else (float32 session.map.size.x) / aspectRatioScreen
        
        TransformComponent.updateWorldMatrices session.scene
        CameraComponent.updateCameraMatrices session.scene aspectRatioScreen
        TileComponent.update session.scene getSprite
        EntityComponent.update session.scene getSprite

    let draw session graphicsDevice =
        match session.scene |> SceneGraph.tryGetComponent<CameraComponent> "MainCamera" with
        | None -> ()
        | Some cam ->
            SpriteRendererComponent.draw session.scene
            |> SpriteBatch3D.draw graphicsDevice cam.viewMatrix cam.projectionMatrix