namespace OrangeBug.Game

module GameMap =
    open OrangeBug
    open OrangeBug.Grid

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

    let private addDependenciesForTile tile position graph =
        let behavior = Behavior.getTileBehavior tile
        let dependencies = behavior.getDependencies tile
        dependencies |> List.fold
            (fun (g: DependencyGraph) dependency ->
                let target =
                    match dependency with
                    | RelativeMapDependency offset -> position + offset
                    | AbsoluteMapDependency pos -> pos
                g.addEdge position target)
            graph
    

    // Map read access

    let getAt position map =
        let tileEntry = map.tiles.Find position
        {
            position = position
            tile = tileEntry.tile
            entityId = tileEntry.entityId
        }

    let hasEntity id map =
        map.entities.ContainsKey id

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

    let getDependenciesOf position map =
        match map.dependencies.outEdges.TryFind position with
        | Some points -> points
        | None -> Set.empty

    let accessor map = {
        getAt = fun p -> getAt p map
        getEntity = fun id -> getEntity id map
        hasEntity = fun id -> hasEntity id map
        getPlayerId = fun name -> getPlayerId name map
        getPositionsDependentOn = fun pos -> getPositionsDependentOn pos map
        getDependenciesOf = fun pos -> getDependenciesOf pos map
    }
    
    
    // Map mutation functions

    let updateTile position newTile map =
        let newDependencies =
            map.dependencies
            |> DependencyGraph.removeOutEdges position
            |> addDependenciesForTile newTile position
            
        let newTiles =
            map.tiles 
            |> updateTileEntry position (fun entry -> TileEntry.Create entry.entityId newTile)

        { map with tiles = newTiles; dependencies = newDependencies }

    let updateEntity id newEntity map =
        let newEntities = map.entities |> updateEntityEntry id (fun entry ->
            match entry with
            | Some entry -> Some (EntityEntry.Create entry.position newEntity)
            | None -> failwithf "updateEntity failed: There is no entity with ID '%O'" id)
        { map with entities = newEntities }

    let spawnEntity position id newEntity map =
        let newEntities = map.entities |> updateEntityEntry id (fun entry ->
            match entry with 
            | Some _ -> failwithf "spawnEntity failed: There is already an entity with ID '%O'" id
            | None -> Some (EntityEntry.Create position newEntity))

        let newPlayers =
            match newEntity with
            | PlayerEntity player -> map.players |> Map.add player.name id
            | _ -> map.players

        let newTiles =
            map.tiles
            |> updateTileEntry position (fun entry -> TileEntry.WithEntity id entry.tile)

        { map with tiles = newTiles; entities = newEntities; players = newPlayers }

    let despawnEntity position map =
        let entry = map.tiles.Find position

        let entityId =
            match entry.entityId with
            | Some id -> id
            | None -> failwithf "despawnEntity failed: There is no entity at position '%O'" position

        let newPlayers = 
            match map.entities.[entityId].entity with
            | PlayerEntity player -> map.players.Remove player.name
            | _ -> map.players

        let newEntities = map.entities |> updateEntityEntry entityId (fun _ -> None)

        let newTiles =
            map.tiles
            |> updateTileEntry position (fun e -> TileEntry.WithoutEntity e.tile)

        { map with tiles = newTiles; entities = newEntities; players = newPlayers }
        
    let moveEntity oldPosition newPosition map =
        let entry = map.tiles.Find oldPosition

        let entityId =
            match entry.entityId with
            | Some id -> id
            | None -> failwithf "moveEntity failed: There is no entity at position '%O'" oldPosition

        let newEntities = map.entities |> updateEntityEntry entityId (fun entry ->
            Some (EntityEntry.Create newPosition entry.Value.entity))

        let newTiles =
            map.tiles
            |> updateTileEntry oldPosition (fun e -> TileEntry.WithoutEntity e.tile)
            |> updateTileEntry newPosition (fun e -> TileEntry.WithEntity entityId e.tile)

        { map with tiles = newTiles; entities = newEntities }

    let applyEffect (map: GameMapState) effect =
        match effect with
        | TileUpdateEffect e -> map |> updateTile e.position e.tile
        | EntityUpdateEffect e -> map |> updateEntity e.entityId e.entity
        | EntityMoveEffect e -> map |> moveEntity e.oldPosition e.newPosition
        | EntitySpawnEffect e -> map |> spawnEntity e.position e.entityId e.entity
        | EntityDespawnEffect e -> map |> despawnEntity e.position
        | SoundEffect _ -> map

    let create width height =
        let playerId = EntityId.create
        let playerPos = Point.create 1 1
        {
            size = Point.create width height
            players = Map.ofList [ "Player", playerId ]
            entities = Map.ofList [
                playerId, EntityEntry.Create playerPos (PlayerEntity { name = "Player"; orientation = East })
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
