namespace OrangeBug.Game

module Gameplay =
    open OrangeBug
    open OrangeBug.Game.Intent

    let private handleIntent intent (context: IntentContext) =
        match intent with
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
    
    type IntentContext with
        static member Create map = {
            mapState = map
            map = GameMap.accessor map
            prevResult = Accepted []
            recentEvents = []
            doHandleIntent = handleIntent
            gameMapApplyEffect = GameMap.applyEffect
            gameMapCreateAccessor = GameMap.accessor
        }

    let private updateAffectedTiles (action: Point -> IntentContext -> IntentResult) context =
        let eventsToAffectedPoints evs =
            evs
            |> Seq.collect Effect.eventToEffects
            |> Seq.collect (function
                | TileUpdateEffect e -> [ e.position ]
                | DependenciesUpdateEffect _ -> []
                | EntityMoveEffect e -> [ e.oldPosition; e.newPosition ]
                | EntitySpawnEffect e -> [ e.position ]
                | EntityDespawnEffect e -> [ e.position ]
                | EntityUpdateEffect _ -> []
                | SoundEffect _ -> [])

        // Not very functional, but works for now. TODO: Use fold and stuff, avoid mutable
        let (Accepted initialEvents) = context.prevResult
        let mutable bag = eventsToAffectedPoints initialEvents |> Set.ofSeq
        let mutable counter = 0
        let mutable lastAcceptedIntent = context
        let mutable allEvents = []

        let todoPoints = seq {
            for p in bag do
                if lastAcceptedIntent.map.getDependenciesOf p |> Seq.forall (bag.Contains >> not) then
                    yield p
        }

        while bag.Count > 0 do
            if Seq.isEmpty todoPoints || counter > 1000 then
                failwithf "dependency cycle detected while updating tiles"

            counter <- counter + 1
            let current = Seq.head todoPoints

            match action current lastAcceptedIntent with
            | Rejected _ -> ()
            | Accepted events ->
                // Add (1) points affected by the tile update, and (2) points depending on the updated point
                let newContext = Intent.applyEvents lastAcceptedIntent events
                bag <- events |> eventsToAffectedPoints |> Seq.fold (fun b p -> b.Add p) bag
                bag <- newContext.map.getPositionsDependentOn current |> Seq.fold (fun b p -> b.Add p) bag
                lastAcceptedIntent <- newContext
                allEvents <- allEvents @ events
            
            bag <- bag.Remove current
            
        Accepted allEvents

    let processIntent intent map =
        let doIntent (ctx: IntentContext) =
            ctx.HandleIntent intent

        let updateTiles (ctx: IntentContext) =
            updateAffectedTiles 
                (fun p ctx -> ctx.HandleIntent (UpdateTileIntent { position = p }))
                ctx
        
        (IntentContext.Create map) |> (doIntent =&&=> updateTiles)