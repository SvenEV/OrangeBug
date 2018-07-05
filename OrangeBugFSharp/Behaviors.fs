namespace OrangeBug.Game

open OrangeBug

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

module Behavior =
    
    // Infrastructure
    
    let private justAccept (context: IntentContext) _ = context.Accept []
    let private justReject (context: IntentContext) _ = context.Reject
    let private zeroDependencies _ = []
    

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
        tryAttachEntity = fun ctx intent -> ctx.Accept [ ButtonPressedEvent { position = intent.position } ]
        tryDetachEntity = fun ctx intent -> ctx.Accept [ ButtonReleasedEvent { position = intent.position } ]
        update = justAccept
        getDependencies = zeroDependencies
    }

    let InkTileBehavior = {
        tryAttachEntity = fun context intent ->
            let (InkTile inkColor) = (context.map.getAt intent.position).tile
            let _, entity = context.map.getEntity intent.entityToAttach
            match entity with
            | BalloonEntity _ ->
                context.Accept [
                    BalloonColoredEvent { 
                        entityId = intent.entityToAttach
                        inkPosition = intent.position
                        color = inkColor
                    }
                ]
            | _ -> context.Accept []

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
                context.Accept [ BalloonPoppedEvent { entityId = intent.entityToAttach; pinPosition = intent.position } ]
            | PlayerEntity _ -> context.Accept []
            | _ -> context.Reject

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
        // If the gate would close on box detach, the player could not move
        // onto it to keep it open.
        tryDetachEntity = justAccept

        update = fun context intent ->
            let tileInfo = (context.map.getAt intent.position)
            let (GateTile gate) = tileInfo.tile
            let gateState = gate.isOpen
            let (ButtonTile buttonState) = (context.map.getAt gate.triggerPosition).tile
            context.Accept
                (match gateState, buttonState, tileInfo.entityId with
                | false, true, None -> [ GateOpenedEvent { gate = gate; position = tileInfo.position } ]
                | true, false, None -> [ GateClosedEvent { gate = gate; position = tileInfo.position } ]
                | _ -> [])
   
        getDependencies = fun tile ->
            let (GateTile gate) = tile
            [ AbsoluteMapDependency gate.triggerPosition ]
    }

    let TeleporterTileBehavior = {
        tryAttachEntity = justAccept
        tryDetachEntity = justAccept
        getDependencies = zeroDependencies
        update = fun context intent ->
            let tileEntry = context.map.getAt intent.position
            let (TeleporterTile targetPosition) = tileEntry.tile
            match tileEntry.entityId with
            | None -> context.Accept []
            | Some entityId ->
                context.HandleIntent (MoveEntityIntent { 
                    entityId = entityId
                    newPosition = targetPosition
                    force = 1
                })
    }
    

    // Entity behaviors
    
    let PlayerEntityBehavior = {
        tryClearTile = justReject
    }

    let BoxEntityBehavior = {
        tryClearTile = fun context intent ->
            match intent.suggestedPushDirection with
            | None -> context.Reject // box can't just disappear without moving somewhere
            | Some dir ->
                let position, _ = context.map.getEntity intent.entityId
                context.HandleIntent (MoveEntityIntent {
                    entityId = intent.entityId
                    newPosition = position + dir.asPoint
                    force = intent.force
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
        | TeleporterTile _ -> TeleporterTileBehavior

    let getEntityBehavior entity =
        match entity with
        | PlayerEntity _ -> PlayerEntityBehavior
        | BoxEntity _ -> BoxEntityBehavior
        | BalloonEntity _ -> BalloonEntityBehavior
