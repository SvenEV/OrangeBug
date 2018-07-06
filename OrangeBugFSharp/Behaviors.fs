namespace OrangeBug.Game

open OrangeBug

type MapDependency =
    | RelativeMapDependency of offset: Point
    | AbsoluteMapDependency of position: Point

type TileBehavior = {
    tryAttachEntity: AttachEntityToTileIntent -> IntentContext -> IntentContext
    tryDetachEntity: DetachEntityFromTileIntent -> IntentContext -> IntentContext
    update: UpdateTileIntent -> IntentContext -> IntentContext
    getDependencies: Tile -> MapDependency list
}

type EntityBehavior = {
    tryClearTile: ClearEntityFromTileIntent -> IntentContext -> IntentContext
}

module Behavior =

    open Intent
    
    // Infrastructure
    
    let private justAccept _ (context: IntentContext) = context.Accept []
    let private justReject _ (context: IntentContext) = context.Reject
    let private zeroDependencies _ = []
    

    // Tile behaviors

    let WallTileBehavior = {
        tryAttachEntity = justReject
        tryDetachEntity = justReject
        update = justAccept
        getDependencies = zeroDependencies
    }

    let PathTileBehavior = {
        tryAttachEntity = fun intent context ->
            let target = context.map.getAt intent.position
            match target.entityId with
                | None -> context.Accept []
                | Some entityToClear ->
                    context.HandleIntent (ClearEntityFromTileIntent { 
                        entityId = entityToClear
                        suggestedPushDirection = intent.suggestedPushDirection
                        force = intent.force - 1
                    })

        tryDetachEntity = justAccept
        update = justAccept
        getDependencies = zeroDependencies
    }

    let ButtonTileBehavior = {
        tryAttachEntity = fun intent context ->
            let emitEvent (ctx: IntentContext) = ctx.Accept [ ButtonPressedEvent { position = intent.position } ]
            context |> (PathTileBehavior.tryAttachEntity intent =&&=> emitEvent)

        tryDetachEntity = fun intent context ->
            context.Accept [ ButtonReleasedEvent { position = intent.position } ]
        
        update = justAccept
        getDependencies = zeroDependencies
    }

    let InkTileBehavior = {
        tryAttachEntity = fun intent context ->
            let emitEvent ctx =
                let (InkTile inkColor) = (ctx.map.getAt intent.position).tile
                let _, entity = ctx.map.getEntity intent.entityToAttach
                match entity with
                | BalloonEntity _ ->
                    ctx.Accept [
                        BalloonColoredEvent { 
                            entityId = intent.entityToAttach
                            inkPosition = intent.position
                            color = inkColor
                        }
                    ]
                | _ -> ctx.Accept []
            context |> (PathTileBehavior.tryAttachEntity intent =&&=> emitEvent)

        tryDetachEntity = justAccept
        update = justAccept
        getDependencies = zeroDependencies
    }

    let PinTileBehavior = {
        tryAttachEntity = fun intent context ->
            let emitEvent ctx =
                let (PinTile pinColor) = (ctx.map.getAt intent.position).tile
                let _, entity = ctx.map.getEntity intent.entityToAttach
                match entity with
                | BalloonEntity color when color = pinColor ->
                    ctx.Accept [ BalloonPoppedEvent { entityId = intent.entityToAttach; pinPosition = intent.position } ]
                | PlayerEntity _ -> ctx.Accept []
                | _ -> ctx.Reject

            context |> (PathTileBehavior.tryAttachEntity intent =&&=> emitEvent)

        tryDetachEntity = justAccept
        update = justAccept
        getDependencies = zeroDependencies
    }

    let GateTileBehavior = {
        tryAttachEntity = fun intent context ->
            let (GateTile gate) = (context.map.getAt intent.position).tile
            match gate.isOpen with
            | true -> PathTileBehavior.tryAttachEntity intent context
            | false -> WallTileBehavior.tryAttachEntity intent context

        // We must not close the gate here. Consider e.g.
        // [player on path][box on gate][path] (button for gate is off)
        // When the player moves right, first the box moves right.
        // If the gate would close on box detach, the player could not move
        // onto it to keep it open.
        tryDetachEntity = justAccept

        update = fun intent context ->
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
        tryAttachEntity = PathTileBehavior.tryAttachEntity
        tryDetachEntity = justAccept
        getDependencies = zeroDependencies
        update = fun intent context ->
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
    
    let CornerTileBehavior = {
        tryAttachEntity = fun intent context ->
            // TODO: Make sure things can't move in from the wrong side
            let target = context.map.getAt intent.position
            let (CornerTile orientation) = target.tile
            match target.entityId with
                | None -> context.Accept []
                | Some entityToClear ->
                    let pushDirection =
                        match orientation, intent.suggestedPushDirection with
                        | North, Some South -> Some East
                        | North, Some West -> Some North
                        | East, Some West -> Some South
                        | East, Some North -> Some East
                        | South, Some North -> Some West
                        | South, Some East -> Some South
                        | West, Some East -> Some North
                        | West, Some South -> Some West
                        | _ -> None

                    context.HandleIntent (ClearEntityFromTileIntent { 
                        entityId = entityToClear
                        suggestedPushDirection = pushDirection
                        force = intent.force - 1
                    })

        tryDetachEntity = justAccept
        update = justAccept
        getDependencies = zeroDependencies
    }


    // Entity behaviors
    
    let PlayerEntityBehavior = {
        tryClearTile = justReject
    }

    let BoxEntityBehavior = {
        tryClearTile = fun intent context ->
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
        | CornerTile _ -> CornerTileBehavior

    let getEntityBehavior entity =
        match entity with
        | PlayerEntity _ -> PlayerEntityBehavior
        | BoxEntity _ -> BoxEntityBehavior
        | BalloonEntity _ -> BalloonEntityBehavior
