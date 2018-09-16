namespace OrangeBug.Game

open Newtonsoft.Json
open OrangeBug
open Microsoft.FSharp.Reflection
open LoxLib

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
        
        // inEdges: e.g. for a button, all gates triggered by that button
        // outEdges: e.g. for a gate, the button that triggers the gate
        dependencies: Point Graph
    }

    [<JsonProperty("tiles")>]
    member this.TilesForJson =
        this.tiles.AsSeq
        |> Seq.sortBy (fun (p, _) -> p.y, p.x)
        |> Seq.map (fun o -> (snd o).tile)

type MapAccessor = {
    getTile: Point -> Tile * EntityId option
    tryGetEntity: EntityId -> (Point * Entity) option
    getPlayerId: string -> EntityId
    getPositionsDependentOn: Point -> Point Set
    getDependenciesOf: Point -> Point Set
}

type MapAssertion<'a> =
    | AssertTrue of 'a
    | AssertFalse of LogElement list
    static member OfOption errorMessage (o: 'a option) =
        match o with
        | Some v -> AssertTrue v
        | None -> AssertFalse errorMessage

module MapAccess =
    let private domain f = FSharpType.GetFunctionElements (f.GetType()) |> fst

    let getTile p accessor =
        AssertTrue (accessor.getTile p)

    let hasEntity id (accessor: MapAccessor) =
        AssertTrue (
            match accessor.tryGetEntity id with
            | Some _ -> true
            | _ -> false)

    let tryGetEntity id accessor =
        AssertTrue (accessor.tryGetEntity id)

    let tryGetEntityAt p accessor =
        let _, entityId = accessor.getTile p
        let result =
            match entityId with
            | Some id -> accessor.tryGetEntity id |> Option.map (fun (_, entity) -> id, entity)
            | None -> None
        AssertTrue result

    let requireTile (tileType: 'a -> Tile) p accessor =
        let tile, _ = accessor.getTile p
        unwrapAs tileType tile |> MapAssertion.OfOption [LogString (sprintf "Expected tile of type '%O' at %O" (domain tileType) p)]

    let requireTileWithoutEntity (tileType: 'a -> Tile) p accessor =
        let tile, entityId = accessor.getTile p
        let errorMessage = LogString (sprintf "Expected tile of type '%O' without entity at %O" (domain tileType) p)
        match entityId with
        | None -> unwrapAs tileType tile |> MapAssertion.OfOption [errorMessage]
        | _ -> AssertFalse [errorMessage]

    let requireEntity (entityType: 'a -> Entity) entityId accessor =
        let entity = accessor.tryGetEntity entityId
        match entity with
        | None -> AssertFalse [LogString (sprintf "Expected entity of type '%O' with ID '%O' (but didn't exist)" (domain entityType) entityId)]
        | Some (position, state) ->
            unwrapAs entityType state
            |> Option.map (fun e -> position, e)
            |> MapAssertion.OfOption [LogString (sprintf "Expected entity of type '%O' with ID '%O' (but had wrong type)" (domain entityType) entityId)]

    let requireEntityAt (entityType: 'a -> Entity) p accessor =
        let _, entityId = accessor.getTile p
        entityId
        |> Option.bind accessor.tryGetEntity
        |> Option.bind (fun (_, entity) -> unwrapAs entityType entity)
        |> Option.map (fun entity -> entityId.Value, entity)
        |> MapAssertion.OfOption [LogString (sprintf "Expected entity of type '%O' at %O" (domain entityType) p)]

    let requireNoEntityAt p accessor =
        let _, entityId = accessor.getTile p
        match entityId with
        | Some _ -> AssertFalse [LogString(sprintf "Expected no entity at %O" p)]
        | None -> AssertTrue ()

    let requireEntityExists entityId accessor =
        accessor.tryGetEntity entityId
        |> MapAssertion.OfOption [LogString(sprintf "Expected entity with ID '%O' to exist" entityId)]

    let requireEntityExistsAt p accessor =
        let _, entityId = accessor.getTile p
        entityId
        |> Option.bind accessor.tryGetEntity
        |> Option.map (fun (_, e) -> entityId.Value, e)
        |> MapAssertion.OfOption [LogString(sprintf "Expected entity to exist at %O" p)]