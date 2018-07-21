namespace OrangeBug.Game

module Gameplay =
    open OrangeBug
    open OrangeBug.Game.Intent

    let handleIntent intent (context: IntentContext) =
        match intent with
        | NopIntent -> Accepted []
        | UpdateTileIntent intent ->
            let tileToUpdate = context.map.getAt intent.position
            let behavior = Behavior.getTileBehavior tileToUpdate.tile
            behavior.update intent context

        | MovePlayerIntent intent ->
            let playerId = context.map.getPlayerId intent.name
            let playerPos, (PlayerEntity playerState) = context.map.getEntity playerId
            
            let rotatePlayer =
                emit (PlayerRotatedEvent {
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
                match ctx.recentEvents |> List.contains expected with
                | true -> Accepted []
                | false -> Rejected ErrorTrace.Empty

            let attachToTarget (ctx: IntentContext) =
                ctx.HandleIntent (AttachEntityToTileIntent {
                    oldPosition = oldPosition
                    move = intent
                })

            let validateAttached (ctx: IntentContext) =
                let expected = EntityAttachedEvent { entityId = intent.entityId; position = intent.newPosition }
                match ctx.recentEvents |> List.contains expected with
                | true -> Accepted []
                | false -> Rejected ErrorTrace.Empty

            let emitEvent (ctx: IntentContext) =
                // check if entity still exists at target
                // (might have been destroyed during attach, see PinTileBehavior)
                match ctx.map.hasEntity intent.entityId with
                | true ->
                    Accepted [ EntityMovedEvent { 
                        entityId = intent.entityId
                        oldPosition = oldPosition
                        newPosition = intent.newPosition
                    } ]
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
            let entityBehavior = tileInfo.entityId.Value |> context.map.getEntity |> snd |> Behavior.getEntityBehavior
            context |> (entityBehavior.validateDetach intent =&&=> tileBehavior.tryDetachEntity intent)


     // Intent helpers
    
    module IntentContext =
        let create map = {
            mapState = map
            map = GameMap.accessor map
            prevResult = Accepted []
            recentEvents = []
            doHandleIntent = handleIntent
            gameMapApplyEffect = GameMap.applyEffect
            gameMapCreateAccessor = GameMap.accessor
        }
        let createWithAccessor map createAccessor = {
            mapState = map
            map = createAccessor map
            prevResult = Accepted []
            recentEvents = []
            doHandleIntent = handleIntent
            gameMapApplyEffect = GameMap.applyEffect
            gameMapCreateAccessor = createAccessor
        }

    let eventToAffectedPoints ev =
        Effect.eventToEffects ev
        |> Seq.collect (function
            | TileUpdateEffect e -> [ e.position ]
            | DependenciesUpdateEffect _ -> []
            | EntityMoveEffect e -> [ e.oldPosition; e.newPosition ]
            | EntitySpawnEffect e -> [ e.position ]
            | EntityDespawnEffect e -> [ e.position ]
            | EntityUpdateEffect _ -> []
            | SoundEffect _ -> [])

    let rec updateTiles points context =
        // taking only the subgraph with the given points, we only need to handle the leafs
        // (the non-leaf points depend on the leafs and will therefore be updated recursively anyway)
        let leafs = DependencyGraph.findLeafs points context.mapState.dependencies

        let updateTile (ctx: IntentContext, events, newlyAffectedPoints) p =
            match ctx.HandleIntent (UpdateTileIntent { position = p }) with
            | Rejected _ -> ctx, events, newlyAffectedPoints
            | Accepted newEvents ->
                let newContext = Intent.applyEvents ctx newEvents
                let newlyAffectedPoints = Set.unionMany [
                    newlyAffectedPoints
                    newEvents |> Seq.collect eventToAffectedPoints |> Set.ofSeq // tiles affected by events caused by update
                    context.map.getPositionsDependentOn p // tiles depending on the updated tile
                ]
                newContext, events @ newEvents, newlyAffectedPoints

        let updateContext, updateEvents, newlyAffectedPoints =
            Seq.fold updateTile (context, [], Set.empty) leafs

        let transitiveUpdateEvents =
            match newlyAffectedPoints.Count with
            | 0 -> []
            | _ -> updateTiles newlyAffectedPoints updateContext

        updateEvents @ transitiveUpdateEvents
                
