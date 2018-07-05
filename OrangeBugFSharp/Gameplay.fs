namespace OrangeBug.Game

module Gameplay =
    open OrangeBug
    open OrangeBug.Game.Intent

    let private handleIntent context intent =
        match intent with
        | UpdateTileIntent intent ->
            let tileToUpdate = context.map.getAt intent.position
            let behavior = Behavior.getTileBehavior tileToUpdate.tile
            behavior.update context { position = intent.position }

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
                    force = 2
                })
            
            context |> (rotatePlayer =||=> movePlayer)

        | MoveEntityIntent intent ->
            let oldPosition, _ = context.map.getEntity intent.entityId
            let target = context.map.getAt intent.newPosition
            let offset = intent.newPosition - oldPosition

            let validateForce (ctx: IntentContext) =
                if intent.force > 0 then ctx.Accept [] else ctx.Reject

            let clearTarget (ctx: IntentContext) =
                match target.entityId with
                | Some id ->
                    ctx.HandleIntent (ClearEntityFromTileIntent { 
                        entityId = id
                        suggestedPushDirection = offset.asDirection
                        force = intent.force - 1
                    })
                | None -> ctx.Accept []
            
            let detachFromSource (ctx: IntentContext) =
                ctx.HandleIntent (DetachEntityFromTileIntent { position = oldPosition })

            let attachToTarget (ctx: IntentContext) =
                ctx.HandleIntent (AttachEntityToTileIntent {
                    position = intent.newPosition
                    entityToAttach = intent.entityId
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
                =&&=> clearTarget
                =&&=> detachFromSource
                =&&=> attachToTarget
                =&&=> emitEvent)

        | ClearEntityFromTileIntent intent ->
            let _, entity = context.map.getEntity intent.entityId
            let behavior = entity |> Behavior.getEntityBehavior
            behavior.tryClearTile context intent

        | AttachEntityToTileIntent intent ->
            let behavior = (context.map.getAt intent.position).tile |> Behavior.getTileBehavior
            behavior.tryAttachEntity context intent

        | DetachEntityFromTileIntent intent ->
            let behavior = (context.map.getAt intent.position).tile |> Behavior.getTileBehavior
            behavior.tryDetachEntity context { position = intent.position; }


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

    let private traverseDependenciesInteractively (action: Point -> IntentContext -> IntentContext) context initialPoints =
        // Not very functional, but works for now. TODO: Use fold and stuff, avoid mutable
        let mutable bag = Set.ofSeq initialPoints
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
            bag <- bag.Remove current

            // TODO: It bugs me that I can't properly use my composeIndependent function
            let updateResult = action current lastAcceptedIntent

            if updateResult.intentResult = IntentAccepted then
                // TODO: Newly affected tiles are not added to bag here
                bag <- updateResult.map.getPositionsDependentOn current |> Seq.fold (fun b p -> b.Add p) bag
                lastAcceptedIntent <- updateResult
            
        lastAcceptedIntent

    let processIntent intent map =
        let doIntent (ctx: IntentContext) =
            ctx.HandleIntent intent

        let updateAffectedTiles (ctx: IntentContext) =
            ctx.emittedEvents
            |> Seq.collect Effect.eventToEffects
            |> Seq.collect (function
                | TileUpdateEffect e -> [ e.position ]
                | EntityMoveEffect e -> [ e.oldPosition; e.newPosition ]
                | EntitySpawnEffect e -> [ e.position ]
                | EntityDespawnEffect e -> [ e.position ]
                | EntityUpdateEffect _ -> []
                | SoundEffect _ -> [])
            |> traverseDependenciesInteractively
                (fun p ctx -> ctx.HandleIntent (UpdateTileIntent { position = p }))
                ctx
        
        (IntentContext.Create map) |> (doIntent =&&=> updateAffectedTiles)