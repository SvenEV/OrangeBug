namespace OrangeBug

module GameMapTypes =
    open Newtonsoft.Json
    open Grid
    open DependencyGraph
    open TilesEntities

    type TileEntry =
        {
            tile: Tile
            entityId: EntityId option
        }
        static member create entityId tile = { tile = tile; entityId = entityId }
        static member WithEntity entityId tile = { tile = tile; entityId = Some entityId }
        static member WithoutEntity tile = { tile = tile; entityId = None }

    type EntityEntry =
        {
            position: Point
            entity: Entity
        }
        static member create position entity = { position = position; entity = entity }

    type GameMap = 
        {
            size: Point
            [<JsonIgnore>]
            tiles: TileEntry Grid
            entities: Map<EntityId, EntityEntry>
            players: Map<string, EntityId>
            dependencies: DependencyGraph
        }

        [<JsonProperty("tiles")>]
        member this.tilesForJson =
            this.tiles.AsSeq
            |> Seq.sortBy (fun (p, _) -> p.y, p.x)
            |> Seq.map (fun o -> (snd o).tile)

    type MapAccessor = {
        getAt: Point -> TileInfo
        getEntity: EntityId -> Point * Entity
        hasEntity: EntityId -> bool
        getPlayerId: string -> EntityId
        getPositionsDependentOn: Point -> Point Set
    }