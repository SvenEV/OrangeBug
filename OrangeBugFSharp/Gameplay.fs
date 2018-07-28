namespace OrangeBug.Game

module Gameplay =
    open OrangeBug
    open OrangeBug.Game.Intent

    let EntityMoveDuration = GameTimeSpan 2

    let handleIntent intent (context: IntentContext) =
        match intent with
        | NopIntent -> Accepted []
        | UpdateTileIntent intent ->
            // Schedule tile updates for dependent tiles
            let scheduleDependentUpdates (ctx: IntentContext) =
                let scheduledUpdates =
                    context.map.getPositionsDependentOn intent.position
                    |> Seq.map (fun p ->
                        {
                            time = ctx.time
                            duration = GameTimeSpan 0
                            event = IntentScheduledEvent {
                                intent = UpdateTileIntent { position = p }
                                time = ctx.time
                            }
                        })
                    |> List.ofSeq
                Accepted scheduledUpdates

            // Updating fails for locked tiles, but that's ok as those will be updated anyway once unlocked
            let updateSelf (ctx: IntentContext) =
                let tileToUpdate = ctx.map.getAt intent.position
                let behavior = Behavior.getTileBehavior tileToUpdate.tile
                behavior.update intent ctx

            context |> (scheduleDependentUpdates =&&=> updateSelf)


        | MovePlayerIntent intent ->
            let playerId = context.map.getPlayerId intent.name
            let playerPos, (PlayerEntity playerState) = context.map.getEntity playerId
            
            let rotatePlayer =
                emitNow 1 (PlayerRotatedEvent {
                    name = intent.name
                    entityId = playerId
                    player = { playerState with orientation = intent.direction }
                    orientation = intent.direction
                })

            let movePlayer (ctx: IntentContext) =
                ctx.HandleIntent (MoveEntityIntent {
                    entityId = playerId;
                    newPosition = playerPos + intent.direction.asPoint
                    mode = Push 2
                    initiator = Other
                    duration = EntityMoveDuration
                })
            
            context |> (rotatePlayer =||=> movePlayer)

        | MoveEntityIntent intent ->
            let oldPosition, _ = context.map.getEntity intent.entityId

            let validateForce _ =
                match intent.mode with
                | Push 0 -> Rejected ErrorTrace.Empty
                | _ -> Accepted []

            let detachFromSource (ctx: IntentContext) =
                ctx.HandleIntent (DetachEntityFromTileIntent {
                    position = oldPosition
                    move = intent
                })

            let validateDetached (ctx: IntentContext) =
                let expected = EntityDetachedEvent { entityId = intent.entityId; position = oldPosition }
                match ctx.recentEvents |> List.exists (fun ev -> ev.event = expected) with
                | true -> Accepted []
                | false -> Rejected ErrorTrace.Empty

            let attachToTarget (ctx: IntentContext) =
                ctx.HandleIntent (AttachEntityToTileIntent {
                    oldPosition = oldPosition
                    move = intent
                })

            let validateAttached (ctx: IntentContext) =
                let expected = EntityAttachedEvent { entityId = intent.entityId; position = intent.newPosition }
                match ctx.recentEvents |> List.exists (fun ev -> ev.event = expected) with
                | true -> Accepted []
                | false -> Rejected ErrorTrace.Empty

            let emitEvent (ctx: IntentContext) =
                // check if entity still exists at target
                // (might have been destroyed during attach, see PinTileBehavior)
                match ctx.map.hasEntity intent.entityId with
                | true ->
                    ctx |> emitNow EntityMoveDuration.value (EntityMovedEvent { 
                        entityId = intent.entityId
                        oldPosition = oldPosition
                        newPosition = intent.newPosition
                    })
                | false ->
                    Accepted []

            let all = (detachFromSource 
                =&&=> validateDetached
                =&&=> attachToTarget
                =&&=> validateAttached
                =&&=> validateForce
                =&&=> emitEvent)

            let traceError = Intent.trace { attemptedMoves = [ oldPosition, intent.newPosition ] }

            // If 'all' succeeds, '=||=>' will invoke 'traceError' but discard its result and return 'Accepted'
            // If 'all' fails, we will get a useful error trace thanks to 'traceError'
            context |> (all =||=> traceError)

        | ClearEntityFromTileIntent intent ->
            let _, entity = context.map.getEntity intent.entityId
            let behavior = entity |> Behavior.getEntityBehavior
            behavior.tryClearTile intent context

        | AttachEntityToTileIntent intent ->
            let behavior = (context.map.getAt intent.move.newPosition).tile |> Behavior.getTileBehavior
            behavior.tryAttachEntity intent context

        | DetachEntityFromTileIntent intent ->
            let tileInfo = context.map.getAt intent.position
            let tileBehavior = tileInfo.tile |> Behavior.getTileBehavior
            match tileInfo.entityId with
            | None -> Rejected ErrorTrace.Empty
            | Some entityId ->
                let entityBehavior = entityId |> context.map.getEntity |> snd |> Behavior.getEntityBehavior
                context |> (entityBehavior.validateDetach intent =&&=> tileBehavior.tryDetachEntity intent)


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