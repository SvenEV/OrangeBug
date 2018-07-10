namespace OrangeBug.Game

open Newtonsoft.Json
open OrangeBug.Grid
open OrangeBug

type MapDependency =
    | RelativeMapDependency of offset: Point
    | AbsoluteMapDependency of position: Point

type TileEntry =
    {
        tile: Tile
        entityId: EntityId option
        dynamicDependencies: MapDependency list
    }
    static member Create entityId tile deps = { tile = tile; entityId = entityId; dynamicDependencies = deps }
    static member WithEntity entityId tile = { tile = tile; entityId = Some entityId; dynamicDependencies = [] }
    static member WithoutEntity tile = { tile = tile; entityId = None;  dynamicDependencies = [] }

type EntityEntry =
    {
        position: Point
        entity: Entity
    }
    static member Create position entity = { position = position; entity = entity }

type GameMapState = 
    {
        size: Point
        [<JsonIgnore>]
        tiles: TileEntry Grid
        entities: Map<EntityId, EntityEntry>
        players: Map<string, EntityId>
        dependencies: DependencyGraph
    }

    [<JsonProperty("tiles")>]
    member this.TilesForJson =
        this.tiles.AsSeq
        |> Seq.sortBy (fun (p, _) -> p.y, p.x)
        |> Seq.map (fun o -> (snd o).tile)

type MapAccessor = {
    getAt: Point -> TileInfo
    getEntity: EntityId -> Point * Entity
    hasEntity: EntityId -> bool
    getPlayerId: string -> EntityId
    getPositionsDependentOn: Point -> Point Set
    getDependenciesOf: Point -> Point Set
}
