namespace OrangeBug

module GameMap =
    open Grid
    open TilesEntities
    open Behaviors
    open Effects
    open DependencyGraph
    open GameMapTypes
    open IntentsEvents

    let private updateTileEntry position updater (tiles: TileEntry Grid) =
        let tileEntry = tiles.Find position
        tiles.Add position (updater tileEntry)

    let private updateEntityEntry id updater (entities: Map<EntityId, EntityEntry>) =
        let currentEntry = entities.TryFind id
        let newEntry = updater currentEntry
        match currentEntry, newEntry with
        | _, Some newEntry -> entities |> Map.add id newEntry // add/update entity
        | Some _, None -> entities.Remove id // remove entity
        | None, None -> entities

    // TODO: Clarify that this is to update the DependencyGraph
    let addForTile tile position graph =
        let behavior = Behaviors.getTileBehavior tile
        let dependencies = behavior.getDependencies tile
        dependencies |> List.fold
            (fun (g: DependencyGraph) dependency ->
                let target =
                    match dependency with
                    | RelativeMapDependency offset -> position + offset
                    | AbsoluteMapDependency pos -> pos
                addEdge position target g)
            graph

    let getAt position map =
        let tileEntry = map.tiles.Find position
        {
            position = position
            tile = tileEntry.tile
            entityId = tileEntry.entityId
        }

    let getEntity id map =
        match map.entities.TryFind id with
        | Some entry -> entry.position, entry.entity
        | None -> failwithf "getEntity failed: There is no entity with ID '%O'" id
        
    let getPlayerId name map =
        map.players.[name]
        
    let getPositionsDependentOn position map =
        match map.dependencies.inEdges.TryFind position with
        | Some points -> points
        | None -> Set.empty

    // Mutation functions
    // (these update the dependency graph but do not cause any tile updates or other intents)
    let updateTile position newTile map =
        let newDependencies =
            map.dependencies
            |> removeOutEdges position
            |> addForTile newTile position
            
        let newTiles =
            map.tiles 
            |> updateTileEntry position (fun entry -> TileEntry.create entry.entityId newTile)

        { map with tiles = newTiles; dependencies = newDependencies }

    let updateEntity id newEntity map =
        let newEntities = map.entities |> updateEntityEntry id (fun entry ->
            match entry with
            | Some entry -> Some (EntityEntry.create entry.position newEntity)
            | None -> failwithf "updateEntity failed: There is no entity with ID '%O'" id)
        { map with entities = newEntities }

    let spawnEntity position newEntity map =
        let id = EntityId.create
            
        let newEntities = map.entities |> updateEntityEntry id (fun entry ->
            match entry with 
            | Some _ -> failwithf "spawnEntity failed: There is already an entity with ID '%O'" id
            | None -> Some (EntityEntry.create position newEntity))

        let newPlayers =
            match newEntity with
            | PlayerEntity player -> map.players |> Map.add player.name id
            | _ -> map.players

        let newTiles =
            map.tiles
            |> updateTileEntry position (fun entry -> TileEntry.WithEntity id entry.tile)

        { map with tiles = newTiles; entities = newEntities; players = newPlayers }

    let despawnEntity id map =
        let entry = map.entities.[id]
            
        let newPlayers = 
            match entry.entity with
            | PlayerEntity player -> map.players.Remove player.name
            | _ -> map.players

        let newEntities = map.entities |> updateEntityEntry id (fun entry ->
            match entry with
            | Some _ -> None
            | None -> failwithf "despawnEntity failed: There is no entity with ID '%O'" id)

        let newTiles =
            map.tiles 
            |> updateTileEntry entry.position (fun e -> TileEntry.WithoutEntity e.tile)

        { map with tiles = newTiles; entities = newEntities; players = newPlayers }
        
    let moveEntity newPosition id map =
        let entry = map.entities.[id]

        let newEntities = map.entities |> updateEntityEntry id (fun entry ->
            match entry with
            | Some entry -> Some (EntityEntry.create newPosition entry.entity)
            | None -> failwithf "moveEntity failed: There is no entity with ID '%O'" id)

        let newTiles =
            map.tiles
            |> updateTileEntry entry.position (fun e -> TileEntry.WithoutEntity e.tile)
            |> updateTileEntry newPosition (fun e -> TileEntry.WithEntity id e.tile)

        { map with tiles = newTiles; entities = newEntities }

    let applyEffect (map: GameMap) effect =
        match effect with
        | TileUpdateEffect e -> map |> updateTile e.position e.tile
        | EntityUpdateEffect e -> map |> updateEntity e.entityId e.entity
        | EntityMoveEffect e -> map |> moveEntity e.newPosition e.entityId
        | EntitySpawnEffect e -> map |> spawnEntity e.position e.entity
        | EntityDespawnEffect e -> map |> despawnEntity e.entityId
        | SoundEffect e -> map

    let accessor map = {
        getAt = fun p -> getAt p map
        getEntity = fun id -> getEntity id map
        getPlayerId = fun name -> getPlayerId name map
        getPositionsDependentOn = fun pos -> getPositionsDependentOn pos map
    }

    let rec applyEvent (map: GameMap) event =
        let effects = Effects.eventToEffects (accessor map) event
        List.fold applyEffect map effects

    and accept context events =
        let newMap = events |> Seq.fold applyEvent context.mapState
        createIntentContext newMap (context.emittedEvents @ events) IntentAccepted

    and reject context events =
        let newMap = events |> Seq.fold applyEvent context.mapState
        createIntentContext newMap (context.emittedEvents @ events) IntentRejected

    and private createIntentContext map events intentResult = {
        mapState = map
        map = accessor map
        emittedEvents = events
        intentResult = intentResult

        doHandleIntent = Gameplay.handleIntent
        acceptIntent = accept
        rejectIntent = reject
    }

    type IntentContext with
        static member create map = createIntentContext map [] IntentAccepted

    type GameMap with
        static member create width height =
            let playerId = EntityId.create
            let playerPos = Point.create 1 1
            {
                size = Point.create width height
                players = Map.ofList [ "Player", playerId ]
                entities = Map.ofList [
                    playerId, EntityEntry.create playerPos (PlayerEntity { name = "Player"; orientation = East })
                ]
            
                tiles = Grid.init (Point.create width height) (fun p ->
                    match p with
                    | p when p = playerPos -> TileEntry.WithEntity playerId PathTile
                    | Point (0, _) -> TileEntry.WithoutEntity WallTile
                    | Point (_, 0) -> TileEntry.WithoutEntity WallTile
                    | Point (x, _) when x = width - 1 -> TileEntry.WithoutEntity WallTile
                    | Point (_, y) when y = height - 1 -> TileEntry.WithoutEntity WallTile
                    | _ -> TileEntry.WithoutEntity PathTile)

                dependencies = DependencyGraph.empty
            }
