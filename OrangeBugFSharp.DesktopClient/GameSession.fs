namespace OrangeBug.DesktopClient

open System
open LoxLib
open OrangeBug
open OrangeBug.Game
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type GameSession = {
    id: string
    mutable scene: SceneGraph
    mutable simulation: Simulation
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
            match session.scene |> SceneGraph.tryGet (EntityComponent.nodeId id) with
            | Some node ->
                match node |> SceneNode.tryGetComponent<EntityComponent>, node |> SceneNode.tryGetComponent<TransformComponent> with
                | Some comp, Some transform -> updater comp transform
                | _ -> ()
            | None -> ()

        match ev.event with
        | PlayerRotatedEvent e -> updateEntityComponent e.entityId (fun c _ -> c.entity <- PlayerEntity e.player)
        | EntityMovedEvent e -> updateEntityComponent e.entityId (fun c t -> c.positionAnimation <- Animation.create t.worldMatrix.Translation (e.newPosition.AsVector3 1.0f) ev.time ev.duration)
        | GateOpenedEvent e -> updateTileComponent e.position (fun c -> c.tile <- GateTile e.gate)
        | GateClosedEvent e -> updateTileComponent e.position (fun c -> c.tile <- GateTile e.gate)
        | BalloonPoppedEvent e -> session.scene <- session.scene |> SceneGraph.remove (EntityComponent.nodeId e.entityId)
        | BalloonColoredEvent e ->
            updateEntityComponent e.entityId (fun c _ -> c.entity <- BalloonEntity e.balloon)
            updateTileComponent e.inkPosition (fun c -> c.tile <- PathTile)
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
        let simulation = Simulation.create SampleMaps.sampleMap1
        {
            id = id
            simulation = simulation
            scene = buildScene simulation.map
            lastTimeSync = TimeSpan.Zero, SimTime 0
            tickTargetTime = float32 Simulation.TickTargetTime.TotalSeconds
        }

    let gameToSimTime session (gameTime: GameTime) =
        let syncGameTime, syncSimTime = session.lastTimeSync
        float32 syncSimTime.value + (float32 <| gameTime.TotalGameTime.TotalSeconds - syncGameTime.TotalSeconds) / session.tickTargetTime
    
    let update session getSprite aspectRatioScreen gameTime =
        let requestMove dir =
            let sim = session.simulation
            let intent = MovePlayerIntent { name = "Player"; direction = dir }
            session.simulation <- {
                sim with
                    scheduledIntents = sim.scheduledIntents @ [ { intent = intent; time = sim.time + (SimTimeSpan 1) } ]
            }
        
        if Keyboard.GetState().IsKeyDown(Keys.Right) then requestMove East
        if Keyboard.GetState().IsKeyDown(Keys.Left) then requestMove West
        if Keyboard.GetState().IsKeyDown(Keys.Up) then requestMove North
        if Keyboard.GetState().IsKeyDown(Keys.Down) then requestMove South
        
        // advance simulation according to passed time
        let simTime = gameToSimTime session gameTime

        let advance (sim, evs) _ =
            let newSim, newEvents = Simulation.advance sim
            newSim, evs @ newEvents

        let newSim, events =
            [session.simulation.time.value .. int simTime - 1]
            |> Seq.fold advance (session.simulation, [])

        if Keyboard.GetState().IsKeyDown(Keys.F1) && (int simTime <> session.simulation.time.value) then
            LoxServer.launchBrowser()

        session.simulation <- newSim
        events |> Seq.iter (handleEvent session)

        // update camera size so map fits onto screen
        match session.scene |> SceneGraph.tryGetComponent<CameraComponent> "MainCamera" with
        | None -> ()
        | Some cam ->
            cam.size <-
                let mapSize = session.simulation.map.size
                let aspectRatioMap = (float32 mapSize.x) / float32 mapSize.y
                if aspectRatioScreen > aspectRatioMap
                    then float32 mapSize.y
                    else (float32 mapSize.x) / aspectRatioScreen
        
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