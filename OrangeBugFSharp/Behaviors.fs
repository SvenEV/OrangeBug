namespace OrangeBug.Game
open OrangeBug

#nowarn "25" // incomplete pattern matching

type TileBehavior = {
    // Should return an 'EntityAttachedEvent' to indicate success
    tryAttachEntity: AttachEntityToTileIntent -> IntentContext -> IntentResult

    // Should return an 'EntityDetachedEvent' to indicate success
    tryDetachEntity: DetachEntityFromTileIntent -> IntentContext -> IntentResult

    update: UpdateTileIntent -> IntentContext -> IntentResult
    getStaticDependencies: Tile -> MapDependency list
}

type EntityBehavior = {
    validateDetach: DetachEntityFromTileIntent -> IntentContext -> IntentResult
    tryClearTile: ClearEntityFromTileIntent -> IntentContext -> IntentResult
}

module Behavior =

    open Intent
    
    // Infrastructure
    
    let private justAccept _ _ = Accepted []
    let private justReject _ _ = Rejected ErrorTrace.Empty
    let private zeroDependencies _ = []


    // Tile behaviors

    let WallTileBehavior = {
        tryAttachEntity = justReject
        tryDetachEntity = justReject
        update = justAccept
        getStaticDependencies = zeroDependencies
    }

    let LockedTileBehavior = {
        WallTileBehavior with
            update = justReject // practically, accepting or rejecting doesn't make a difference in 'update'
            getStaticDependencies = zeroDependencies
    }

    let PathTileBehavior = {
        WallTileBehavior with

            tryAttachEntity = fun intent context ->
                let clearTargetTile (ctx: IntentContext) =
                    let target = context.map.getAt intent.move.newPosition
                    match target.entityId with
                    | None -> Accepted []
                    | Some entityToClear ->
                        let moveDirection =
                            match intent.move.mode with
                            | Teleport -> None
                            | Push _ -> (intent.move.newPosition - intent.oldPosition).asDirection
                        ctx.HandleIntent (ClearEntityFromTileIntent { 
                            entityId = entityToClear
                            suggestedPushDirection = moveDirection
                            move = intent.move
                        })
                        
                let emitEvent = emit intent.move.duration 0 (EntityAttachedEvent { entityId = intent.move.entityId; position = intent.move.newPosition })
                context |> (clearTargetTile =&&=> emitEvent)
                
            tryDetachEntity = fun intent ->
                emitNow 0 (EntityDetachedEvent { entityId = intent.move.entityId; position = intent.position })        
    }

    let ButtonTileBehavior = {
        WallTileBehavior with

            tryAttachEntity = fun intent context ->
                let emitEvent (ctx: IntentContext) =
                    let (ButtonTile button) = (ctx.map.getAt intent.move.newPosition).tile
                    ctx |> emit intent.move.duration 0 (ButtonPressedEvent { position = intent.move.newPosition; button = { button with isPressed = true } })
                context |> (PathTileBehavior.tryAttachEntity intent =&&=> emitEvent)

            tryDetachEntity = fun intent context ->
                let emitEvent (ctx: IntentContext) =
                    let (ButtonTile button) = (ctx.map.getAt intent.position).tile
                    ctx |> emitNow 0 (ButtonReleasedEvent { position = intent.position; button = { button with isPressed = false } })
                context |> (PathTileBehavior.tryDetachEntity intent =&&=> emitEvent)
    }

    let InkTileBehavior = {
        PathTileBehavior with        
            tryAttachEntity = fun intent context ->
                let emitEvent ctx =
                    let (InkTile inkTile) = (ctx.map.getAt intent.move.newPosition).tile
                    let _, entity = ctx.map.getEntity intent.move.entityId
                    match entity with
                    | BalloonEntity balloon ->
                        ctx |> emit intent.move.duration 0 (BalloonColoredEvent { 
                            entityId = intent.move.entityId
                            inkPosition = intent.move.newPosition
                            color = inkTile.color
                            balloon = { balloon with color = inkTile.color }
                        })
                    | _ -> Accepted []
                context |> (PathTileBehavior.tryAttachEntity intent =&&=> emitEvent)
    }

    let PinTileBehavior = {
        // Why not to emit 'BalloonPoppedEvent' in 'tryAttachEntity':
        // * event would be emitted prior to 'EntityMovedEvent', resulting 'TileUpdateEffect' would fail
        // * if we load a map where initially there's a balloon on a pin, it wouldn't pop

        PathTileBehavior with

            update = fun intent context ->
                let tileEntry = context.map.getAt intent.position
                let (PinTile pin) = tileEntry.tile
                match tileEntry.entityId with
                | None -> Accepted []
                | Some entityId ->
                    let _, entity = context.map.getEntity entityId
                    match entity with
                    | BalloonEntity balloon when balloon.color = pin.color ->
                        context |> emitNow 0 (BalloonPoppedEvent { entityId = entityId; pinPosition = intent.position })
                    | _ -> Accepted []

            tryAttachEntity = fun intent context ->
                let validateEntity ctx =
                    let (PinTile pinTile) = (ctx.map.getAt intent.move.newPosition).tile
                    let _, entity = ctx.map.getEntity intent.move.entityId
                    match entity with
                    | BalloonEntity balloon when balloon.color = pinTile.color -> Accepted []
                    | PlayerEntity _ -> Accepted []
                    | _ -> Rejected ErrorTrace.Empty
                context |> (validateEntity =&&=> PathTileBehavior.tryAttachEntity intent)
    }

    let GateTileBehavior = {
        PathTileBehavior with

            tryAttachEntity = fun intent context ->
                let (GateTile gate) = (context.map.getAt intent.move.newPosition).tile
                match gate.isOpen with
                | true -> PathTileBehavior.tryAttachEntity intent context
                | false -> WallTileBehavior.tryAttachEntity intent context

            // We must NOT close the gate in 'tryDetachEntity' or 'onEntityDetached'.
            // Consider e.g. [player on path][box on gate][path] (button for gate is off)
            // When the player moves right, first the box moves right.
            // If the gate would close on box detach, the player could
            // not move onto it to keep it open.
            update = fun intent context ->
                let tileInfo = (context.map.getAt intent.position)
                let (GateTile gate) = tileInfo.tile
                let (ButtonTile button) = (context.map.getAt gate.triggerPosition).tile
                match gate.isOpen, button.isPressed, tileInfo.entityId with
                | false, true, None -> context |> emitNow 4 (GateOpenedEvent { gate = { gate with isOpen = true }; position = tileInfo.position })
                | true, false, None -> context |> emitNow 4 (GateClosedEvent { gate = { gate with isOpen = false }; position = tileInfo.position })
                | _ -> Accepted []
       
            getStaticDependencies = fun tile ->
                let (GateTile gate) = tile
                [ AbsoluteMapDependency gate.triggerPosition ]
    }

    let TeleporterTileBehavior = {
        PathTileBehavior with
            update = fun intent context ->
                let tileEntry = context.map.getAt intent.position
                let (TeleporterTile teleporter) = tileEntry.tile
                match teleporter.isActive with
                | true ->
                    // teleport entity (if target is again a teleporter, temporarily deactivate)
                    match tileEntry.entityId with
                    | None -> Accepted []
                    | Some entityId ->
                        let doTeleport (ctx: IntentContext) =
                            ctx.HandleIntent (MoveEntityIntent { 
                                entityId = entityId
                                newPosition = teleporter.targetPosition
                                mode = Teleport
                                initiator = SomeTeleporter
                                duration = GameTimeSpan 4
                            })
                        
                        let deactivateTargetTeleporter (ctx: IntentContext) =
                            let targetTile = ctx.map.getAt teleporter.targetPosition
                            match targetTile.tile with
                            | TeleporterTile targetTeleporter ->
                                ctx |> emitNow 0 (TeleporterDeactivatedEvent {
                                    position = teleporter.targetPosition
                                    teleporter = { targetTeleporter with isActive = false }
                                })
                            | _ -> Accepted []
                        
                        context |> (doTeleport =&&=> deactivateTargetTeleporter)
                | false ->
                    // do not teleport but re-activate teleporter
                    context |> emitNow 0 (TeleporterActivatedEvent { 
                        position = intent.position
                        teleporter = { teleporter with isActive = true }
                    })
    }
    
    let CornerTileBehavior = {
        WallTileBehavior with

            tryAttachEntity = fun intent context ->
                let target = context.map.getAt intent.move.newPosition
                let (CornerTile corner) = target.tile
                let inDirection = (intent.move.newPosition - intent.oldPosition).asDirection
                let outDirection = CornerTile.mapInToOutDirection corner.orientation inDirection

                // ensure entity can't move in from a wall side of the corner (but allow teleports)
                let validateEntry _ =
                    match intent.move.mode, outDirection with
                    | Push _, None _ -> Rejected ErrorTrace.Empty
                    | _ -> Accepted []
                
                // empty the target tile, suggesting a move around the corner
                let clearTargetTile (ctx: IntentContext) =
                    match target.entityId with
                        | None -> Accepted []
                        | Some entityToClear ->
                            ctx.HandleIntent (ClearEntityFromTileIntent { 
                                entityId = entityToClear
                                suggestedPushDirection = outDirection
                                move = intent.move
                            })
                 
                let emitEvent = emit intent.move.duration 0 (EntityAttachedEvent { entityId = intent.move.entityId; position = intent.move.newPosition })
                context |> (validateEntry =&&=> clearTargetTile =&&=> emitEvent)

            tryDetachEntity = fun intent context ->
                let validateExit (ctx: IntentContext) =
                    let target = context.map.getAt intent.position
                    let (CornerTile corner) = target.tile
                    let outDirection = (intent.move.newPosition - intent.position).asDirection
                    let validOutDir = CornerTile.isValidOutDirection corner.orientation outDirection
                    match intent.move.mode, validOutDir with
                    | Push _, false -> Rejected ErrorTrace.Empty
                    | _ -> Accepted []
                
                let emitEvent = emitNow 0 (EntityDetachedEvent { entityId = intent.move.entityId; position = intent.position })
                context |> (validateExit =&&=> emitEvent)
    }

    let PistonTileBehavior = {
        WallTileBehavior with

            tryAttachEntity = fun intent context ->
                let _, entity = context.map.getEntity intent.move.entityId
                match entity with
                | PistonEntity _ -> context |> emit intent.move.duration 0 (EntityAttachedEvent { entityId = intent.move.entityId; position = intent.move.newPosition })
                | _ -> Rejected ErrorTrace.Empty

            tryDetachEntity = fun intent context ->
                let _, entity = context.map.getEntity intent.move.entityId
                match entity with
                | PistonEntity _ -> context |> emitNow 0 (EntityDetachedEvent { entityId = intent.move.entityId; position = intent.position })
                | _ -> Rejected ErrorTrace.Empty

            update = fun intent context ->
                let tileInfo = context.map.getAt intent.position
                let (PistonTile piston) = tileInfo.tile
                
                let extendPiston (ctx: IntentContext) =
                    let tryExtend (ctx: IntentContext) =
                        let pistonEntity =
                            match tileInfo.entityId with
                            | Some id -> id
                            | None -> failwithf "Missing PistonEntity on PistonTile at '%O' while trying to extend" intent.position
                        ctx.HandleIntent (MoveEntityIntent {
                            entityId = pistonEntity
                            newPosition = intent.position + piston.orientation.asPoint
                            mode = Push piston.force
                            initiator = SomePiston
                            duration = GameTimeSpan 1
                        })

                    let emitEvent = emitNow 4 (PistonExtendedEvent { position = intent.position; piston = { piston with isExtended = true } })

                    match ctx |> (tryExtend =&&=> emitEvent) with
                    | Accepted events -> Accepted events
                    | Rejected trace ->
                        // register dependencies on all tiles along the "push path" so that if something
                        // changes there, 'update' is called again and the piston can try extending again
                        let deps = trace.attemptedMoves |> List.map (snd >> AbsoluteMapDependency)
                        ctx |> emitNow 0 (DependenciesUpdatedEvent { position = intent.position; newDependencies = deps })

                let retractPiston (ctx: IntentContext) =
                    let tryRetract (ctx: IntentContext) =
                        let neighborPosition = intent.position + piston.orientation.asPoint
                        let neighborTileInfo = context.map.getAt neighborPosition
                        let pistonEntity =
                            match tileInfo.entityId, neighborTileInfo.entityId with
                            | Some id, _ -> id
                            | None, Some id -> id
                            | None, None -> failwithf "Missing PistonEntity on PistonTile at '%O' or '%O' while trying to retract" intent.position neighborPosition
                        ctx.HandleIntent (MoveEntityIntent {
                            entityId = pistonEntity
                            newPosition = intent.position
                            mode = Push 1 // no need for stronger force here (by the way, TODO: Do we need a Pull-mode?)
                            initiator = SomePiston
                            duration = GameTimeSpan 1
                        })

                    let emitEvent = emitNow 4 (PistonRetractedEvent { position = intent.position; piston = { piston with isExtended = false } })
                    let removeDependencies = emitNow 0 (DependenciesUpdatedEvent { position = intent.position; newDependencies = [] })
                    ctx |> (tryRetract =&&=> emitEvent =&&=> removeDependencies)
                
                let isTriggerOn = (context.map.getAt piston.triggerPosition).tile |> function
                    | ButtonTile b -> b.isPressed
                    | _ -> false
                
                match piston.isExtended, isTriggerOn with
                | false, true -> context |> extendPiston
                | true, false -> context |> retractPiston
                | _ -> Accepted []

            getStaticDependencies = fun tile ->
                let (PistonTile piston) = tile
                [ AbsoluteMapDependency piston.triggerPosition ]
    }


    // Entity behaviors
    
    let PlayerEntityBehavior = {
        validateDetach = justAccept
        tryClearTile = justReject
    }

    let BoxEntityBehavior = {
        validateDetach = justAccept
        tryClearTile = fun intent context ->
            match intent.move.mode, intent.suggestedPushDirection with
            | _, None -> Rejected ErrorTrace.Empty // box can't just disappear without moving somewhere
            | Teleport, _ -> Rejected ErrorTrace.Empty // box can't be pushed away through teleportation
            | Push force, Some dir ->
                let position, _ = context.map.getEntity intent.entityId
                context.HandleIntent (MoveEntityIntent {
                    entityId = intent.entityId
                    newPosition = position + dir.asPoint
                    mode = Push (force - 1)
                    initiator = intent.move.initiator
                    duration = intent.move.duration
                })
    }

    let BalloonEntityBehavior = {
        validateDetach = justAccept
        tryClearTile = BoxEntityBehavior.tryClearTile
    }

    let PistonEntityBehavior = {
        validateDetach = fun intent context ->
            // piston entity can only be moved by piston tile (e.g. it can't be teleported)
            match intent.move.initiator with
            | SomePiston -> Accepted []
            | _ -> Rejected ErrorTrace.Empty

        tryClearTile = justReject
    }

    let getTileBehavior tile =
        match tile with
        | PathTile _ -> PathTileBehavior
        | WallTile _ -> WallTileBehavior
        | LockedTile _ -> LockedTileBehavior
        | InkTile _ -> InkTileBehavior
        | PinTile _ -> PinTileBehavior
        | ButtonTile _ -> ButtonTileBehavior
        | GateTile _ -> GateTileBehavior
        | TeleporterTile _ -> TeleporterTileBehavior
        | CornerTile _ -> CornerTileBehavior
        | PistonTile _ -> PistonTileBehavior

    let getEntityBehavior entity =
        match entity with
        | PlayerEntity _ -> PlayerEntityBehavior
        | BoxEntity _ -> BoxEntityBehavior
        | BalloonEntity _ -> BalloonEntityBehavior
        | PistonEntity _ -> PistonEntityBehavior
