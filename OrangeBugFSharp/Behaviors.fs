namespace OrangeBug.Game

open OrangeBug

type MapDependency =
    | RelativeMapDependency of offset: Point
    | AbsoluteMapDependency of position: Point

type TileBehavior = {
    tryAttachEntity: AttachEntityToTileIntent -> IntentContext -> IntentResult
    tryDetachEntity: DetachEntityFromTileIntent -> IntentContext -> IntentResult
    update: UpdateTileIntent -> IntentContext -> IntentResult
    getDependencies: Tile -> MapDependency list
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
        getDependencies = zeroDependencies
    }

    let PathTileBehavior = {
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

            let emitEvent = emit (EntityAttachedEvent { entityId = intent.move.entityId; position = intent.move.newPosition })
            context |> (clearTargetTile =&&=> emitEvent)
            
        tryDetachEntity = fun intent ->
            emit (EntityDetachedEvent { entityId = intent.move.entityId; position = intent.position })
        
        update = justAccept
        getDependencies = zeroDependencies
    }

    let ButtonTileBehavior = {
        tryAttachEntity = fun intent context ->
            let emitEvent = emit (ButtonPressedEvent { position = intent.move.newPosition })
            context |> (PathTileBehavior.tryAttachEntity intent =&&=> emitEvent)

        tryDetachEntity = fun intent context ->
            let emitEvent = emit (ButtonReleasedEvent { position = intent.position })
            context |> (PathTileBehavior.tryDetachEntity intent =&&=> emitEvent)
        
        update = justAccept
        getDependencies = zeroDependencies
    }

    let InkTileBehavior = {
        tryAttachEntity = fun intent context ->
            let emitEvent ctx =
                let (InkTile inkColor) = (ctx.map.getAt intent.move.newPosition).tile
                let _, entity = ctx.map.getEntity intent.move.entityId
                match entity with
                | BalloonEntity _ ->
                    Accepted [
                        BalloonColoredEvent { 
                            entityId = intent.move.entityId
                            inkPosition = intent.move.newPosition
                            color = inkColor
                        }
                    ]
                | _ -> Accepted []
            context |> (PathTileBehavior.tryAttachEntity intent =&&=> emitEvent)

        tryDetachEntity = PathTileBehavior.tryDetachEntity
        update = justAccept
        getDependencies = zeroDependencies
    }

    let PinTileBehavior = {
        tryAttachEntity = fun intent context ->
            let emitEvent ctx =
                let (PinTile pinColor) = (ctx.map.getAt intent.move.newPosition).tile
                let _, entity = ctx.map.getEntity intent.move.entityId
                match entity with
                | BalloonEntity color when color = pinColor ->
                    // TODO: Do we need to emit EntityMovedEvent before BalloonPoppedEvent?
                    Accepted [ BalloonPoppedEvent { entityId = intent.move.entityId; pinPosition = intent.move.newPosition } ]
                | PlayerEntity _ -> Accepted []
                | _ -> Rejected ErrorTrace.Empty
            context |> (PathTileBehavior.tryAttachEntity intent =&&=> emitEvent)

        tryDetachEntity = PathTileBehavior.tryDetachEntity
        update = justAccept
        getDependencies = zeroDependencies
    }

    let GateTileBehavior = {
        tryAttachEntity = fun intent context ->
            let (GateTile gate) = (context.map.getAt intent.move.newPosition).tile
            match gate.isOpen with
            | true -> PathTileBehavior.tryAttachEntity intent context
            | false -> WallTileBehavior.tryAttachEntity intent context

        // We must not close the gate here. Consider e.g.
        // [player on path][box on gate][path] (button for gate is off)
        // When the player moves right, first the box moves right.
        // If the gate would close on box detach, the player could not move
        // onto it to keep it open.
        tryDetachEntity = PathTileBehavior.tryDetachEntity

        update = fun intent context ->
            let tileInfo = (context.map.getAt intent.position)
            let (GateTile gate) = tileInfo.tile
            let gateState = gate.isOpen
            let (ButtonTile buttonState) = (context.map.getAt gate.triggerPosition).tile
            Accepted
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
        tryDetachEntity = PathTileBehavior.tryDetachEntity
        getDependencies = zeroDependencies
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
                        })
                    
                    let deactivateTargetTeleporter (ctx: IntentContext) =
                        let targetTile = ctx.map.getAt teleporter.targetPosition
                        match targetTile.tile with
                        | TeleporterTile targetTeleporter ->
                            Accepted [
                                TeleporterDeactivatedEvent {
                                    position = teleporter.targetPosition
                                    teleporter = targetTeleporter
                                }
                            ]
                        | _ -> Accepted []
                    
                    context |> (doTeleport =&&=> deactivateTargetTeleporter)
            | false ->
                // do not teleport but re-activate teleporter
                Accepted [
                    TeleporterActivatedEvent { 
                        position = intent.position
                        teleporter = teleporter
                    }
                ]
    }
    
    let CornerTileBehavior = {
        tryAttachEntity = fun intent context ->
            let target = context.map.getAt intent.move.newPosition
            let (CornerTile orientation) = target.tile
            let inDirection = (intent.move.newPosition - intent.oldPosition).asDirection
            let outDirection = CornerTile.mapInToOutDirection orientation inDirection

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
             
            let emitEvent = emit (EntityAttachedEvent { entityId = intent.move.entityId; position = intent.move.newPosition })
            context |> (validateEntry =&&=> clearTargetTile =&&=> emitEvent)

        tryDetachEntity = fun intent context ->
            let validateExit (ctx: IntentContext) =
                let target = context.map.getAt intent.position
                let (CornerTile orientation) = target.tile
                let outDirection = (intent.move.newPosition - intent.position).asDirection
                let validOutDir = CornerTile.isValidOutDirection orientation outDirection
                match intent.move.mode, validOutDir with
                | Push _, false -> Rejected ErrorTrace.Empty
                | _ -> Accepted []
            
            let emitEvent = emit (EntityDetachedEvent { entityId = intent.move.entityId; position = intent.position })
            context |> (validateExit =&&=> emitEvent)

        update = justAccept
        getDependencies = zeroDependencies
    }

    let PistonTileBehavior = {
        tryAttachEntity = fun intent context ->
            let _, entity = context.map.getEntity intent.move.entityId
            match entity with
            | PistonEntity _ -> Accepted [ EntityAttachedEvent { entityId = intent.move.entityId; position = intent.move.newPosition } ]
            | _ -> Rejected ErrorTrace.Empty

        tryDetachEntity = fun intent context ->
            let _, entity = context.map.getEntity intent.move.entityId
            match entity with
            | PistonEntity _ -> Accepted [ EntityDetachedEvent { entityId = intent.move.entityId; position = intent.position }]
            | _ -> Rejected ErrorTrace.Empty

        update = fun intent context ->
            let tileInfo = context.map.getAt intent.position
            let (PistonTile piston) = tileInfo.tile
            
            let extendPiston (ctx: IntentContext) =
                let pistonEntity =
                    match tileInfo.entityId with
                    | Some id -> id
                    | None -> failwithf "Missing PistonEntity on PistonTile at '%O' while trying to extend" intent.position
                ctx.HandleIntent (MoveEntityIntent {
                    entityId = pistonEntity
                    newPosition = intent.position + piston.orientation.asPoint
                    mode = Push piston.force
                    initiator = SomePiston
                })

            let retractPiston (ctx: IntentContext) =
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
                })
            
            let isTriggerOn = (context.map.getAt piston.triggerPosition).tile |> function
                | ButtonTile b -> b
                | _ -> false
            
            match piston.isExtended, isTriggerOn with
            | false, true -> context |> (extendPiston =&&=> emit (PistonExtendedEvent { position = intent.position; piston = piston }))
            | true, false -> context |> (retractPiston =&&=> emit (PistonRetractedEvent { position = intent.position; piston = piston }))
            | _ -> Accepted []

        getDependencies = fun tile ->
            // Pistons have a dependency on their trigger and on all tiles along the push direction
            // that could cause a held-back piston to extend if an entity detaches.
            // TODO: This won't work if CornerTiles are involved (and produces unnecessarily many
            // dependencies if there's a WallTile in the "line of sight")! Do we need some kind of
            // raycast mechanism? Or should we just put dependencies onto all tiles in a certain radius?
            let (PistonTile piston) = tile
            let neighborDeps = [1 .. piston.force] |> List.map (fun i -> RelativeMapDependency (i * piston.orientation.asPoint))
            let triggerDep = AbsoluteMapDependency piston.triggerPosition
            triggerDep :: neighborDeps
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
