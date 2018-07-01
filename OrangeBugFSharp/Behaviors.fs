namespace OrangeBug

module Behaviors =
    open IntentsEvents
    open TilesEntities
    
    // Infrastructure

    type MapDependency =
        | RelativeMapDependency of offset: Point
        | AbsoluteMapDependency of position: Point

    type TileBehavior = {
        tryAttachEntity: IntentContext -> AttachEntityToTileIntent -> IntentContext
        tryDetachEntity: IntentContext -> DetachEntityFromTileIntent -> IntentContext
        update: IntentContext -> UpdateAfterDependencyChangedIntent -> IntentContext
        getDependencies: Tile -> MapDependency list
    }

    type EntityBehavior = {
        tryClearTile: IntentContext -> ClearEntityFromTileIntent -> IntentContext
    }
    
    let justAccept (context: IntentContext) _ = context.accept []
    let justReject (context: IntentContext) _ = context.reject []
    let zeroDependencies _ = []
    

    // Tile behaviors

    let WallTileBehavior = {
        tryAttachEntity = justReject
        tryDetachEntity = justReject
        update = justAccept
        getDependencies = zeroDependencies
    }

    let PathTileBehavior = {
        tryAttachEntity = justAccept
        tryDetachEntity = justAccept
        update = justAccept
        getDependencies = zeroDependencies
    }

    let ButtonTileBehavior = {
        tryAttachEntity = fun map args -> map.accept [ ButtonPressedEvent { position = args.position } ]
        tryDetachEntity = fun map args -> map.accept [ ButtonReleasedEvent { position = args.position } ]
        update = justAccept
        getDependencies = zeroDependencies
    }

    let GateTileBehavior = {
        tryAttachEntity = fun context intent ->
            let (GateTile gate) = (context.map.getAt intent.position).tile
            match gate.isOpen with
            | true -> PathTileBehavior.tryAttachEntity context intent
            | false -> WallTileBehavior.tryAttachEntity context intent

        tryDetachEntity = fun context intent ->
            // If gate still open but button not pressed, close gate
            let tileInfo = (context.map.getAt intent.position)
            let (GateTile gate) = tileInfo.tile
            let gateState = gate.isOpen
            let (ButtonTile buttonState) = (context.map.getAt gate.triggerPosition).tile
            context.accept
                (match gateState, buttonState with
                | true, false -> [ GateClosedEvent { position = tileInfo.position } ]
                | _ -> [])

        update = fun context intent ->
            let tileInfo = (context.map.getAt intent.position)
            let (GateTile gate) = tileInfo.tile
            let gateState = gate.isOpen
            let (ButtonTile buttonState) = (context.map.getAt gate.triggerPosition).tile
            context.accept
                (match gateState, buttonState, tileInfo.entityId with
                | false, true, None -> [ GateOpenedEvent { position = tileInfo.position } ]
                | true, false, None -> [ GateClosedEvent { position = tileInfo.position } ]
                | _ -> [])
   
        getDependencies = fun tile ->
            let (GateTile gate) = tile
            [ AbsoluteMapDependency gate.triggerPosition ]
    }
    

    // Entity behaviors
    
    let PlayerEntityBehavior = {
        tryClearTile = justReject
    }

    let BoxEntityBehavior = {
        tryClearTile = fun context intent ->
            match intent.suggestedPushDirection with
            | None -> context.reject [] // box can't just disappear without moving somewhere
            | Some dir ->
                let position, _ = context.map.getEntity intent.entityId
                context.handleIntent (MoveEntityIntent {
                    entityId = intent.entityId
                    newPosition = position + dir.asPoint
                })
    }

    let getTileBehavior tile =
        match tile with
        | PathTile _ -> PathTileBehavior
        | WallTile _ -> WallTileBehavior
        | InkTile _ -> WallTileBehavior
        | PinTile _ -> WallTileBehavior
        | ButtonTile _ -> ButtonTileBehavior
        | GateTile _ -> GateTileBehavior

    let getEntityBehavior entity =
        match entity with
        | PlayerEntity _ -> PlayerEntityBehavior
        | BoxEntity _ -> BoxEntityBehavior
        | BalloonEntity _ -> PlayerEntityBehavior
