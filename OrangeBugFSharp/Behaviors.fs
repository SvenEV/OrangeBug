namespace OrangeBug

module Behaviors =
    open IntentsEvents
    open TilesEntities


    type TileInfo = {
        tile: Tile
        entity: Entity option
        position: Point
    }

    type MapAccessor = {
        getAt: Point -> TileInfo
        getPlayerPosition: string -> Point
        getPlayer: string -> Point * PlayerEntity

        handleIntent: MapAccessor -> Intent -> IntentResult
    }

    type TryAttachEntityArgs = {
        tileInfo: TileInfo
        entityToAttach: Entity
        map: MapAccessor
    }

    type TryDetachEntityArgs = {
        tileInfo: TileInfo
        map: MapAccessor
    }

    type TryClearTileArgs = {
        tileInfo: TileInfo
        suggestedPushDirection: Direction option
        map: MapAccessor
    }

    type TileBehavior = {
        tryAttachEntity: TryAttachEntityArgs -> IntentResult
        tryDetachEntity: TryDetachEntityArgs -> IntentResult
    }

    type EntityBehavior = {
        tryClearTile: TryClearTileArgs -> IntentResult
    }
    
    
    // Tile behaviors
    let WallTileBehavior = {
        tryAttachEntity = fun _ -> IntentRejected []
        tryDetachEntity = fun _ -> IntentRejected []
    }

    let PathTileBehavior = {
        tryAttachEntity = fun _ -> IntentAccepted []
        tryDetachEntity = fun _ -> IntentAccepted []
    }

    let GateTileBehavior = {
        tryAttachEntity = fun args ->
            let (GateTile gate) = args.tileInfo.tile
            match gate.isOpen with
            | true -> PathTileBehavior.tryAttachEntity args
            | false -> WallTileBehavior.tryAttachEntity args

        tryDetachEntity = fun _ -> IntentAccepted []
    }
    
    // Entity behaviors
    let PlayerEntityBehavior = {
        tryClearTile = fun _ -> IntentRejected []
    }

    let BoxEntityBehavior = {
        tryClearTile = fun args ->
            match args.suggestedPushDirection with
            | None -> IntentRejected [] // box can't just disappear without moving somewhere
            | Some dir -> args.map.handleIntent args.map (MoveEntityIntent {
                    sourcePosition = args.tileInfo.position
                    targetPosition = args.tileInfo.position + dir.asPoint
                })
    }

    let getTileBehavior tile =
        match tile with
        | PathTile _ -> PathTileBehavior
        | WallTile _ -> WallTileBehavior
        | InkTile _ -> WallTileBehavior
        | PinTile _ -> WallTileBehavior
        | ButtonTile _ -> WallTileBehavior
        | GateTile _ -> GateTileBehavior

    let getEntityBehavior entity =
        match entity with
        | PlayerEntity _ -> PlayerEntityBehavior
        | BoxEntity _ -> BoxEntityBehavior
        | BalloonEntity _ -> PlayerEntityBehavior
