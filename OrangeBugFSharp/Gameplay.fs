namespace OrangeBug.Game

module Gameplay =
    open OrangeBug
    open OrangeBug.Game.Intent

    let EntityMoveDuration = SimTimeSpan 2

    let handleIntent intent context = gameplay context {
        match intent with
        | NopIntent -> return Accepted []
        | UpdateTileIntent intent ->
            // Schedule tile updates for dependent tiles
            let scheduleDependentUpdates ctx =
                let scheduledUpdates =
                    ctx.map.getPositionsDependentOn intent.position
                    |> Seq.map (fun p ->
                        {
                            time = ctx.time
                            duration = SimTimeSpan 0
                            event = IntentScheduledEvent {
                                intent = UpdateTileIntent { position = p }
                                time = ctx.time
                            }
                        })
                    |> List.ofSeq
                Accepted scheduledUpdates

            // Updating fails for locked tiles, but that's ok as those will be updated anyway once unlocked
            let updateSelf ctx = gameplay ctx {
                let! tileToUpdate, _ = MapAccess.getTile intent.position
                let behavior = Behavior.ofTile tileToUpdate
                return! behavior.update intent
            }

            return! scheduleDependentUpdates =&&=> updateSelf

        | MovePlayerIntent intent ->
            let playerId = context.map.getPlayerId intent.name
            let! playerPos, playerState = MapAccess.requireEntity PlayerEntity playerId
            
            let rotatePlayer =
                emitNow 1 (PlayerRotatedEvent {
                    name = intent.name
                    entityId = playerId
                    player = { playerState with orientation = intent.direction }
                    orientation = intent.direction
                })

            let movePlayer =
                handle (MoveEntityIntent {
                    entityId = playerId;
                    newPosition = playerPos + intent.direction.asPoint
                    mode = Push 2
                    initiator = Other
                    duration = EntityMoveDuration
                })
            
            return! rotatePlayer =&&=> attempt movePlayer

        | MoveEntityIntent intent ->
            let! oldPosition, _ = MapAccess.requireEntityExists intent.entityId

            let validateForce _ =
                match intent.mode with
                | Push 0 -> Rejected (ErrorTrace.Log "No force remaining to push entity")
                | _ -> Accepted []

            let detachFromSource =
                handle (DetachEntityFromTileIntent {
                    position = oldPosition
                    move = intent
                })

            let validateDetached =
                requireRecentEvent (EntityDetachedEvent { entityId = intent.entityId; position = oldPosition })

            let attachToTarget =
                handle (AttachEntityToTileIntent {
                    oldPosition = oldPosition
                    move = intent
                })

            let validateAttached =
                requireRecentEvent (EntityAttachedEvent { entityId = intent.entityId; position = intent.newPosition })

            let emitEvent ctx = gameplay ctx {
                return! emitNow intent.duration.value (EntityMovedEvent { 
                    entityId = intent.entityId
                    oldPosition = oldPosition
                    newPosition = intent.newPosition
                })
            }

            let all = (detachFromSource 
                =&&=> validateDetached
                =&&=> attachToTarget
                =&&=> validateAttached
                =&&=> validateForce
                =&&=> attempt emitEvent)

            let traceError = trace {
                attemptedMoves = [ oldPosition, intent.newPosition ]
                log = sprintf "Failed to move entity from '%O' to '%O'" oldPosition intent.newPosition
            }

            // If 'all' succeeds, '=||=>' will invoke 'traceError' but discard its result and return 'Accepted'
            // If 'all' fails, we will get a useful error trace thanks to 'traceError'
            return! all =||=> traceError

        | ClearEntityFromTileIntent intent ->
            let! _, entity = MapAccess.requireEntityExists intent.entityId
            return! (Behavior.ofEntity entity).tryClearTile intent

        | AttachEntityToTileIntent intent ->
            let! tile, _ = MapAccess.getTile intent.move.newPosition
            return! (Behavior.ofTile tile).tryAttachEntity intent

        | DetachEntityFromTileIntent intent ->
            let! tile, _ = MapAccess.getTile intent.position
            let! _, entity = MapAccess.requireEntityExistsAt intent.position
            return! (Behavior.ofEntity entity).validateDetach intent
                =&&=> (Behavior.ofTile tile).tryDetachEntity intent
    }
    

    // Intent helpers

    let eventToAffectedPoints ev =
        match ev with
        | EntityAttachedEvent e -> [ e.position ]
        | EntityDetachedEvent e -> [ e.position ]
        | _ ->
            Effect.eventToEffects ev
            |> List.collect (function
                | TileUpdateEffect e -> [ e.position ]
                | DependenciesUpdateEffect _ -> []
                | EntityMoveEffect e -> []
                | EntitySpawnEffect e -> [ e.position ]
                | EntityDespawnEffect e -> [ e.position ]
                | EntityUpdateEffect _ -> []
                | SoundEffect _ -> [])

    let createContext map time = {
        mapState = map
        map = GameMap.accessor map
        events = []
        recentEvents = []
        time = time
        doHandleIntent = handleIntent
        gameMapApplyEffect = GameMap.applyEffect
        gameMapCreateAccessor = GameMap.accessor
    }
    
    let createContextWithAccessor map time createAccessor = {
        mapState = map
        map = createAccessor map
        events = []
        recentEvents = []
        time = time
        doHandleIntent = handleIntent
        gameMapApplyEffect = GameMap.applyEffect
        gameMapCreateAccessor = createAccessor
    }