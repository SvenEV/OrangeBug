namespace OrangeBug.Game

module Gameplay =
    open OrangeBug
    open OrangeBug.Game.Intent

    let private handleIntent context intent =
        match intent with
        | UpdateTileIntent intent ->
            let tileToUpdate = context.map.getAt intent.position
            let behavior = Behavior.getTileBehavior tileToUpdate.tile
            behavior.update { position = intent.position } context

        | MovePlayerIntent intent ->
            let playerId = context.map.getPlayerId intent.name
            let playerPos, (PlayerEntity playerState) = context.map.getEntity playerId
            
            let rotatePlayer (ctx: IntentContext) =
                ctx.Accept [ PlayerRotatedEvent {
                    name = intent.name
                    entityId = playerId
                    player = playerState
                    orientation = intent.direction
                } ]

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
            let target = context.map.getAt intent.newPosition
            let offset = intent.newPosition - oldPosition

            let validateForce (ctx: IntentContext) =
                match intent.mode with
                | Push 0 -> ctx.Reject
                | _ -> ctx.Accept []

            let detachFromSource (ctx: IntentContext) =
                ctx.HandleIntent (DetachEntityFromTileIntent {
                    position = oldPosition
                    move = intent
                })

            let attachToTarget (ctx: IntentContext) =
                ctx.HandleIntent (AttachEntityToTileIntent {
                    oldPosition = oldPosition
                    move = intent
                })

            let emitEvent (ctx: IntentContext) =
                // check if entity still exists at target
                // (might have been destroyed during attach, see PinTileBehavior)
                match ctx.map.hasEntity intent.entityId with
                | true ->
                    ctx.Accept [ EntityMovedEvent { 
                        entityId = intent.entityId
                        oldPosition = oldPosition
                        newPosition = intent.newPosition
                    } ]
                | false ->
                    ctx.Accept []

            context |> (validateForce
                =&&=> detachFromSource
                =&&=> attachToTarget
                =&&=> emitEvent)

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

    let rec private accept context events =
        let newMap = events |> Seq.collect Effect.eventToEffects |> Seq.fold GameMap.applyEffect context.mapState
        createIntentContext newMap (context.emittedEvents @ events) IntentAccepted

    and private reject context =
        createIntentContext context.mapState context.emittedEvents IntentRejected

    and private createIntentContext map events intentResult = {
        mapState = map
        map = GameMap.accessor map
        emittedEvents = events
        intentResult = intentResult

        doHandleIntent = handleIntent
        acceptIntent = accept
        rejectIntent = reject
    }

    type IntentContext with
        static member Create map = createIntentContext map [] IntentAccepted

    let private updateAffectedTiles (action: Point -> IntentContext -> IntentContext) context =

        let eventsToAffectedPoints evs =
            evs
            |> Seq.collect Effect.eventToEffects
            |> Seq.collect (function
                | TileUpdateEffect e -> [ e.position ]
                | EntityMoveEffect e -> [ e.oldPosition; e.newPosition ]
                | EntitySpawnEffect e -> [ e.position ]
                | EntityDespawnEffect e -> [ e.position ]
                | EntityUpdateEffect _ -> []
                | SoundEffect _ -> [])

        // Not very functional, but works for now. TODO: Use fold and stuff, avoid mutable
        let mutable bag = eventsToAffectedPoints context.emittedEvents |> Set.ofSeq
        let mutable counter = 0
        let mutable lastAcceptedIntent = context

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

            let updateResult = action current lastAcceptedIntent
            let updateEvents =
                Set.difference
                    (Set.ofSeq updateResult.emittedEvents)
                    (Set.ofSeq lastAcceptedIntent.emittedEvents)

            if updateResult.intentResult = IntentAccepted then
                // Add (1) points affected by the tile update, and (2) points depending on the updated point
                bag <- updateEvents |> eventsToAffectedPoints |> Seq.fold (fun b p -> b.Add p) bag
                bag <- updateResult.map.getPositionsDependentOn current |> Seq.fold (fun b p -> b.Add p) bag
                lastAcceptedIntent <- updateResult
            
            bag <- bag.Remove current
            
        lastAcceptedIntent

    let processIntent intent map =
        let doIntent (ctx: IntentContext) =
            ctx.HandleIntent intent

        let updateTiles (ctx: IntentContext) =
            updateAffectedTiles 
                (fun p ctx -> ctx.HandleIntent (UpdateTileIntent { position = p }))
                ctx
        
        (IntentContext.Create map) |> (doIntent =&&=> updateTiles)