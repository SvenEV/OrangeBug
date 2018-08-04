namespace OrangeBug.Game
open OrangeBug

type TileBehavior = {
    // Should return an 'EntityAttachedEvent' to indicate success
    tryAttachEntity: AttachEntityToTileIntent -> IntentContext -> IntentResult

    // Should return an 'EntityDetachedEvent' to indicate success
    tryDetachEntity: DetachEntityFromTileIntent -> IntentContext -> IntentResult

    // Accepting or rejecting doesn't make a difference here
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
    let private justReject msg _ _ = Rejected (ErrorTrace.Log msg)
    let private zeroDependencies _ = []


    // Tile behaviors

    let WallTileBehavior = {
        tryAttachEntity = justReject "Entities can't attach to this tile"
        tryDetachEntity = justReject "Entities can't detaach from this tile"
        update = justAccept
        getStaticDependencies = zeroDependencies
    }

    let LockedTileBehavior = WallTileBehavior

    let PathTileBehavior = {
        WallTileBehavior with

            tryAttachEntity = fun intent context -> gameplay context {
                let clearTargetTile ctx = gameplay ctx {
                    let! _, entityId = MapAccess.getTile intent.move.newPosition
                    match entityId with
                    | None -> return Accepted []
                    | Some entityToClear ->
                        let moveDirection =
                            match intent.move.mode with
                            | Teleport -> None
                            | Push _ -> (intent.move.newPosition - intent.oldPosition).asDirection
                        return! ClearEntityFromTileIntent { 
                            entityId = entityToClear
                            suggestedPushDirection = moveDirection
                            move = intent.move
                        }
                }
                        
                let emitEvent =
                    EntityAttachedEvent { entityId = intent.move.entityId; position = intent.move.newPosition }
                    |> emit intent.move.duration 0 
                
                return! clearTargetTile =&&=> emitEvent
            }
                
            tryDetachEntity = fun intent ->
                emitNow 0 (EntityDetachedEvent { entityId = intent.move.entityId; position = intent.position })        
    }

    let ButtonTileBehavior = {
        PathTileBehavior with
            update = fun intent context -> gameplay context {
                let! button = MapAccess.requireTile ButtonTile intent.position
                let! entity = MapAccess.tryGetEntityAt intent.position
                match button.isPressed, entity with
                | false, Some _ -> return! emitNow 0 (ButtonPressedEvent { position = intent.position; button = { button with isPressed = true } })
                | true, None -> return! emitNow 0 (ButtonReleasedEvent { position = intent.position; button = { button with isPressed = false } })
                | _ -> return Accepted []
            }
    }

    let InkTileBehavior = {
        PathTileBehavior with
            update = fun intent context -> gameplay context {
                let! inkTile = MapAccess.requireTile InkTile intent.position
                let! id, balloon = MapAccess.requireEntityAt BalloonEntity intent.position
                return! emitNow 0 (BalloonColoredEvent { 
                    entityId = id
                    inkPosition = intent.position
                    color = inkTile.color
                    balloon = { balloon with color = inkTile.color }
                })
            }
    }

    let PinTileBehavior = {
        // Why not to emit 'BalloonPoppedEvent' in 'tryAttachEntity':
        // * event would be emitted prior to 'EntityMovedEvent', resulting 'TileUpdateEffect' would fail
        // * if we load a map where initially there's a balloon on a pin, it wouldn't pop
        PathTileBehavior with

            update = fun intent context -> gameplay context {
                let! pinTile = MapAccess.requireTile PinTile intent.position
                let! id, balloon = MapAccess.requireEntityAt BalloonEntity intent.position
                if balloon.color = pinTile.color
                    then return! emitNow 0 (BalloonPoppedEvent { entityId = id; pinPosition = intent.position })
                    else return Accepted []
            }

            tryAttachEntity = fun intent context -> gameplay context {
                let validateEntity ctx = gameplay ctx {
                    let! pinTile = MapAccess.requireTile PinTile intent.move.newPosition
                    let! _, entity = MapAccess.requireEntityExists intent.move.entityId
                    match entity with
                    | BalloonEntity balloon when balloon.color = pinTile.color -> return Accepted []
                    | PlayerEntity _ -> return Accepted []
                    | _ -> return Rejected (ErrorTrace.Log (sprintf "Not allowed on PinTile: %O" entity))
                }
                return! validateEntity =&&=> PathTileBehavior.tryAttachEntity intent
            }
    }

    let GateTileBehavior = {
        PathTileBehavior with

            tryAttachEntity = fun intent context -> gameplay context {
                let! gate = MapAccess.requireTile GateTile intent.move.newPosition
                match gate.isOpen with
                | true -> return! PathTileBehavior.tryAttachEntity intent
                | false -> return! WallTileBehavior.tryAttachEntity intent
            }

            // We must NOT close the gate in 'tryDetachEntity' or 'onEntityDetached'.
            // Consider e.g. [player on path][box on gate][path] (button for gate is off)
            // When the player moves right, first the box moves right.
            // If the gate would close on box detach, the player could
            // not move onto it to keep it open.
            update = fun intent context -> gameplay context {
                let! gate = MapAccess.requireTileWithoutEntity GateTile intent.position
                let! trigger, _ = MapAccess.getTile gate.triggerPosition
                let isTriggerOn =
                    match trigger with
                    | ButtonTile b -> b.isPressed
                    | _ -> false
                
                match gate.isOpen, isTriggerOn with
                | false, true -> return! emitNow 4 (GateOpenedEvent { gate = { gate with isOpen = true }; position = intent.position })
                | true, false -> return! emitNow 4 (GateClosedEvent { gate = { gate with isOpen = false }; position = intent.position })
                | _ -> return Accepted []
            }
       
            getStaticDependencies = fun tile ->
                let (GateTile gate) = tile
                [ AbsoluteMapDependency gate.triggerPosition ]
    }

    let TeleporterTileBehavior = {
        PathTileBehavior with
            update = fun intent context -> gameplay context {
                let! teleporter = MapAccess.requireTile TeleporterTile intent.position
                if teleporter.isActive then
                    // teleport entity (if target is again a teleporter, temporarily deactivate)
                    let! entityId, _ = MapAccess.requireEntityExistsAt intent.position

                    let doTeleport ctx = gameplay ctx {
                        return! MoveEntityIntent { 
                            entityId = entityId
                            newPosition = teleporter.targetPosition
                            mode = Teleport
                            initiator = SomeTeleporter
                            duration = GameTimeSpan 4
                        }
                    }
                    
                    let deactivateTargetTeleporter ctx = gameplay ctx {
                        let! targetTeleporter = MapAccess.requireTile TeleporterTile teleporter.targetPosition
                        return! emitNow 0 (TeleporterDeactivatedEvent {
                            position = teleporter.targetPosition
                            teleporter = { targetTeleporter with isActive = false }
                        })
                    }
                    
                    return! doTeleport =&&=> attempt deactivateTargetTeleporter
                else
                    // do not teleport but re-activate teleporter
                    return! emitNow 0 (TeleporterActivatedEvent { 
                        position = intent.position
                        teleporter = { teleporter with isActive = true }
                    })
            }
    }
    
    let CornerTileBehavior = {
        WallTileBehavior with

            tryAttachEntity = fun intent context -> gameplay context {
                let! corner = MapAccess.requireTile CornerTile intent.move.newPosition
                let inDirection = (intent.move.newPosition - intent.oldPosition).asDirection
                let outDirection = CornerTile.mapInToOutDirection corner.orientation inDirection

                // ensure entity can't move in from a wall side of the corner (but allow teleports)
                let validateEntry _ =
                    match intent.move.mode, outDirection with
                    | Push _, None -> Rejected (ErrorTrace.Log "Tried to enter CornerTile from a wrong side")
                    | _ -> Accepted []
                
                // empty the target tile, suggesting a move around the corner
                let clearTargetTile ctx = gameplay ctx {
                    let! _, entity = MapAccess.getTile intent.move.newPosition
                    match entity with
                    | None -> return Accepted []
                    | Some entityToClear ->
                        return! ClearEntityFromTileIntent { 
                            entityId = entityToClear
                            suggestedPushDirection = outDirection
                            move = intent.move
                        }
                }
                 
                let emitEvent =
                    EntityAttachedEvent { entityId = intent.move.entityId; position = intent.move.newPosition }
                    |> emit intent.move.duration 0
                
                return! validateEntry =&&=> clearTargetTile =&&=> emitEvent
            }

            tryDetachEntity = fun intent context -> gameplay context {
                let validateExit ctx = gameplay ctx {
                    let outDirection = (intent.move.newPosition - intent.position).asDirection
                    let! corner = MapAccess.requireTile CornerTile intent.position
                    let isValidOutDir = CornerTile.isValidOutDirection corner.orientation outDirection
                    match intent.move.mode, isValidOutDir with
                    | Push _, false -> return Rejected (ErrorTrace.Log "Tried to leave CornerTile via wrong side")
                    | _ -> return Accepted []
                }
                
                let emitEvent =
                    EntityDetachedEvent { entityId = intent.move.entityId; position = intent.position }
                    |> emitNow 0
                
                return! validateExit =&&=> emitEvent
            }
    }

    let PistonTileBehavior = {
        WallTileBehavior with

            tryAttachEntity = fun intent context -> gameplay context {
                let! _ = MapAccess.requireEntity PistonEntity intent.move.entityId
                return! emit intent.move.duration 0 (EntityAttachedEvent { entityId = intent.move.entityId; position = intent.move.newPosition })
            }

            tryDetachEntity = fun intent context -> gameplay context {
                let! _ = MapAccess.requireEntity PistonEntity intent.move.entityId
                return! emitNow 0 (EntityDetachedEvent { entityId = intent.move.entityId; position = intent.position })
            }

            update = fun intent context -> gameplay context {
                
                let extendPiston ctx = gameplay ctx {
                    let! piston = MapAccess.requireTile PistonTile intent.position
                    
                    let tryExtend ctx = gameplay ctx {
                        let! pistonEntity, _ = MapAccess.requireEntityAt PistonEntity intent.position
                        return! MoveEntityIntent {
                            entityId = pistonEntity
                            newPosition = intent.position + piston.orientation.asPoint
                            mode = Push piston.force
                            initiator = SomePiston
                            duration = GameTimeSpan 1
                        }
                    }

                    let emitEvent =
                        PistonExtendedEvent { position = intent.position; piston = { piston with isExtended = true } }
                        |> emitNow 4

                    let! result = tryExtend =&&=> emitEvent
                    match result with
                    | Accepted events -> return Accepted events
                    | Rejected trace ->
                        // register dependencies on all tiles along the "push path" so that if something
                        // changes there, 'update' is called again and the piston can try extending again
                        let deps = trace.attemptedMoves |> List.map (snd >> AbsoluteMapDependency)
                        return! emitNow 0 (DependenciesUpdatedEvent { position = intent.position; newDependencies = deps })
                }
                
                let retractPiston ctx = gameplay ctx {
                    let! piston = MapAccess.requireTile PistonTile intent.position

                    let tryRetract ctx = gameplay ctx {
                        do! MapAccess.requireNoEntityAt intent.position
                        let neighborPosition = intent.position + piston.orientation.asPoint
                        let! pistonEntity, _ = MapAccess.requireEntityAt PistonEntity neighborPosition
                        return! MoveEntityIntent {
                            entityId = pistonEntity
                            newPosition = intent.position
                            mode = Push 1 // no need for stronger force here (by the way, TODO: Do we need a Pull-mode?)
                            initiator = SomePiston
                            duration = GameTimeSpan 1
                        }
                    }

                    let emitEvent = emitNow 4 (PistonRetractedEvent { position = intent.position; piston = { piston with isExtended = false } })
                    let removeDependencies = emitNow 0 (DependenciesUpdatedEvent { position = intent.position; newDependencies = [] })
                    return! tryRetract =&&=> emitEvent =&&=> removeDependencies
                }
                
                let! piston = MapAccess.requireTile PistonTile intent.position                        
                let! trigger, _ = MapAccess.getTile piston.triggerPosition
                let isTriggerOn =
                    match trigger with
                    | ButtonTile b -> b.isPressed
                    | _ -> false
                
                match piston.isExtended, isTriggerOn with
                | false, true -> return! extendPiston
                | true, false -> return! retractPiston
                | _ -> return Accepted []
            }

            getStaticDependencies = fun tile ->
                let (PistonTile piston) = tile
                [ AbsoluteMapDependency piston.triggerPosition ]
    }


    // Entity behaviors
    
    let PlayerEntityBehavior = {
        validateDetach = justAccept
        tryClearTile = justReject "PlayerEntity can only move on its own - it can't be pushed"
    }

    let BoxEntityBehavior = {
        validateDetach = justAccept
        tryClearTile = fun intent context -> gameplay context {
            match intent.move.mode, intent.suggestedPushDirection with
            | _, None -> return Rejected (ErrorTrace.Log "No push direction suggested - box can't just disappear")
            | Teleport, _ -> return Rejected (ErrorTrace.Log "Box can't be pushed away through teleportation")
            | Push force, Some dir ->
                // For better code reuse, we do not specifically require a BoxEntity here
                let! entity = MapAccess.tryGetEntity intent.entityId
                match entity with
                | None -> return Rejected (ErrorTrace.Log "Missing entity in BoxEntityBehavior.tryClearTile")
                | Some (position, _) ->
                    return! MoveEntityIntent {
                        entityId = intent.entityId
                        newPosition = position + dir.asPoint
                        mode = Push (force - 1)
                        initiator = intent.move.initiator
                        duration = intent.move.duration
                    }
        }
    }

    let BalloonEntityBehavior = {
        validateDetach = justAccept
        tryClearTile = BoxEntityBehavior.tryClearTile
    }

    let PistonEntityBehavior = {
        validateDetach = fun intent _ ->
            // piston entity can only be moved by piston tile (e.g. it can't be teleported)
            match intent.move.initiator with
            | SomePiston -> Accepted []
            | _ -> Rejected (ErrorTrace.Log "Something tried to move PistonEntity")

        tryClearTile = justReject "PistonEntity is operated by PistonTile - it can't be pushed"
    }

    let ofTile tile =
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

    let ofEntity entity =
        match entity with
        | PlayerEntity _ -> PlayerEntityBehavior
        | BoxEntity _ -> BoxEntityBehavior
        | BalloonEntity _ -> BalloonEntityBehavior
        | PistonEntity _ -> PistonEntityBehavior
