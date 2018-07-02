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
        update: IntentContext -> UpdateTileIntent -> IntentContext
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
        tryAttachEntity = fun ctx intent -> ctx.accept [ ButtonPressedEvent { position = intent.position } ]
        tryDetachEntity = fun ctx intent -> ctx.accept [ ButtonReleasedEvent { position = intent.position } ]
        update = justAccept
        getDependencies = zeroDependencies
    }

    let InkTileBehavior = {
        tryAttachEntity = fun context intent ->
            let (InkTile inkColor) = (context.map.getAt intent.position).tile
            let _, entity = context.map.getEntity intent.entityToAttach
            match entity with
            | BalloonEntity color ->
                context.accept [
                    BalloonColoredEvent { 
                        entityId = intent.entityToAttach
                        inkPosition = intent.position
                        color = inkColor
                    }
                ]
            | _ -> context.accept []

        tryDetachEntity = justAccept
        update = justAccept
        getDependencies = zeroDependencies
    }

    let PinTileBehavior = {
        tryAttachEntity = fun context intent ->
            let (PinTile pinColor) = (context.map.getAt intent.position).tile
            let _, entity = context.map.getEntity intent.entityToAttach
            match entity with
            | BalloonEntity color when color = pinColor ->
                context.accept [ BalloonPoppedEvent intent.entityToAttach; ]
            | PlayerEntity _ -> context.accept []
            | _ -> context.reject []

        tryDetachEntity = justAccept
        update = justAccept
        getDependencies = zeroDependencies
    }

    let GateTileBehavior = {
        tryAttachEntity = fun context intent ->
            let (GateTile gate) = (context.map.getAt intent.position).tile
            match gate.isOpen with
            | true -> PathTileBehavior.tryAttachEntity context intent
            | false -> WallTileBehavior.tryAttachEntity context intent

        // We must not close the gate here. Consider e.g.
        // [player on path][box on gate][path] (button for gate is off)
        // When the player moves right, first the box moves right.
        // If the gate closes on box detach, the player could not move onto
        // it to keep it open.
        // 
        // TODO: We have to wait until the intent chain is completed and all
        // affected tiles (and tiles depending on them directly or indirectly) 
        // are updated
        tryDetachEntity = justAccept

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

    let BalloonEntityBehavior = {
        tryClearTile = BoxEntityBehavior.tryClearTile
    }

    let getTileBehavior tile =
        match tile with
        | PathTile _ -> PathTileBehavior
        | WallTile _ -> WallTileBehavior
        | InkTile _ -> InkTileBehavior
        | PinTile _ -> PinTileBehavior
        | ButtonTile _ -> ButtonTileBehavior
        | GateTile _ -> GateTileBehavior

    let getEntityBehavior entity =
        match entity with
        | PlayerEntity _ -> PlayerEntityBehavior
        | BoxEntity _ -> BoxEntityBehavior
        | BalloonEntity _ -> BalloonEntityBehavior
