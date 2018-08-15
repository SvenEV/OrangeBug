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

    let private addDependenciesForTile tile position dynamicDeps graph =
        let behavior = Behavior.ofTile tile
        let dependencies = dynamicDeps @ behavior.getStaticDependencies tile
        dependencies |> List.fold
            (fun (g: Point Graph) dependency ->
                let target =
                    match dependency with
                    | RelativeMapDependency offset -> position + offset
                    | AbsoluteMapDependency pos -> pos
                g.AddEdge position target)
            graph

    // Map read access

    let getTile position map =
        let tileEntry = map.tiles.Find position
        tileEntry.tile, tileEntry.entityId

    let tryGetEntity id map =
        map.entities.TryFind id
        |> Option.map (fun e -> e.position, e.entity)
        
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
        getTile = fun p -> getTile p map
        tryGetEntity = fun id -> tryGetEntity id map
        getPlayerId = fun name -> getPlayerId name map
        getPositionsDependentOn = fun pos -> getPositionsDependentOn pos map
        getDependenciesOf = fun pos -> getDependenciesOf pos map
    }
    
    
    // Map mutation functions

    let updateTile position newTile map =
        let dynamicDeps = (map.tiles.Find position).dynamicDependencies

        let newDependencies =
            map.dependencies
            |> Graph.removeOutEdges position
            |> addDependenciesForTile newTile position dynamicDeps
            
        let newTiles =
            map.tiles 
            |> updateTileEntry position (fun entry -> { entry with tile = newTile })

        { map with tiles = newTiles; dependencies = newDependencies }

    let updateTileDependencies position dynamicDeps map =
        let tile = (map.tiles.Find position).tile
   
        let newDependencies =
            map.dependencies
            |> Graph.removeOutEdges position
            |> addDependenciesForTile tile position dynamicDeps

        let newTiles =
            map.tiles
            |> updateTileEntry position (fun entry -> { entry with dynamicDependencies = dynamicDeps })

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
        | DependenciesUpdateEffect e -> map |> updateTileDependencies e.position e.newDependencies
        | EntityUpdateEffect e -> map |> updateEntity e.entityId e.entity
        | EntityMoveEffect e -> map |> moveEntity e.oldPosition e.newPosition
        | EntitySpawnEffect e -> map |> spawnEntity e.position e.entityId e.entity
        | EntityDespawnEffect e -> map |> despawnEntity e.position
        | SoundEffect _ -> map

    let empty = {
        size = Point.zero
        tiles = Grid.empty Point.zero
        entities = Map.empty
        players = Map.empty
        dependencies = Graph.empty
    }

    let create width height =
        let playerId = EntityId.create
        let playerPos = Point.create 1 1
        {
            size = Point.create width height
            players = Map.ofList [ "Player", playerId ]
            
            entities = Map.ofList
                [
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

            dependencies = Graph.empty
        }
