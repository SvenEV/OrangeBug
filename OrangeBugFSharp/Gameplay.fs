﻿namespace OrangeBug

module Gameplay =
    open IntentsEvents
    open Behaviors
    open Effects

    let private handleIntent context intent =
        match intent with
        | UpdateTileIntent intent ->
            let tileToUpdate = context.map.getAt intent.position
            let behavior = getTileBehavior tileToUpdate.tile
            behavior.update context { position = intent.position }

        | MovePlayerIntent intent ->
            let playerId = context.map.getPlayerId intent.name
            let playerPos, _ = context.map.getEntity playerId
            
            let rotatePlayer (ctx: IntentContext) =
                ctx.Accept [ PlayerRotatedEvent { name = intent.name; orientation = intent.direction } ]

            let movePlayer (ctx: IntentContext) =
                ctx.HandleIntent (MoveEntityIntent {
                    entityId = playerId;
                    newPosition = playerPos + intent.direction.asPoint
                })
            
            context |> (rotatePlayer =||=> movePlayer)

        | MoveEntityIntent intent ->
            let oldPosition, _ = context.map.getEntity intent.entityId
            let target = context.map.getAt intent.newPosition
            let offset = intent.newPosition - oldPosition

            let clearTarget (ctx: IntentContext) =
                match target.entityId with
                | Some id ->
                    ctx.HandleIntent (ClearEntityFromTileIntent { 
                        entityId = id
                        suggestedPushDirection = offset.asDirection
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
                        oldPosition = oldPosition
                        newPosition = intent.newPosition
                    } ]
                | false ->
                    ctx.Accept []

            context |> (clearTarget
                =&&=> detachFromSource
                =&&=> attachToTarget
                =&&=> emitEvent)

        | ClearEntityFromTileIntent intent ->
            let _, entity = context.map.getEntity intent.entityId
            let behavior = entity |> Behaviors.getEntityBehavior
            behavior.tryClearTile context intent

        | AttachEntityToTileIntent intent ->
            let behavior = (context.map.getAt intent.position).tile |> Behaviors.getTileBehavior
            behavior.tryAttachEntity context intent

        | DetachEntityFromTileIntent intent ->
            let behavior = (context.map.getAt intent.position).tile |> Behaviors.getTileBehavior
            behavior.tryDetachEntity context { position = intent.position; }


     // Intent helpers

    let rec private accept context events =
        let newMap = events |> Seq.fold GameMap.applyEvent context.mapState
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

    let traverseDependenciesInteractively (action: Point -> IntentContext -> IntentContext) context initialPoints =
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
                bag <- updateResult.map.getPositionsDependentOn current |> Seq.fold (fun b p -> b.Add p) bag
                lastAcceptedIntent <- updateResult
            
        lastAcceptedIntent

    let processIntent intent map =
        let doIntent (ctx: IntentContext) =
            ctx.HandleIntent intent

        let updateAffectedTiles (ctx: IntentContext) =
            let effects = ctx.emittedEvents |> Seq.collect (eventToEffects (GameMap.accessor map))
            let points =
                effects
                |> Seq.collect (function
                    | TileUpdateEffect e -> [ e.position ]
                    | EntityMoveEffect e -> [ e.oldPosition; e.newPosition ]
                    | EntitySpawnEffect e -> [ e.position ]
                    | EntityDespawnEffect e -> [ e.position ]
                    | EntityUpdateEffect _ -> []
                    | SoundEffect _ -> [])
                |> Set.ofSeq
            
            let updater = traverseDependenciesInteractively (fun p ctx -> ctx.HandleIntent (UpdateTileIntent { position = p }))
            updater ctx points
        
        (IntentContext.Create map) |> (doIntent =&&=> updateAffectedTiles)