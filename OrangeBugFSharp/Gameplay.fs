namespace OrangeBug

module Gameplay =
    open IntentsEvents
    open Behaviors

    let handleIntent context intent =
        match intent with
        | UpdateDependentTilesIntent intent ->
            let handleDependency (ctx: IntentContext) pos =
                let (updateResult: IntentContext) = ctx.HandleIntent (UpdateTileIntent { position = pos; })
                // Failing updates should not fail the whole intent chain
                updateResult.Accept []

            let dependencies = context.map.getPositionsDependentOn intent.position
            dependencies |> Set.fold handleDependency (context.Accept [])

        | UpdateTileIntent intent ->
            let tileToUpdate = context.map.getAt intent.position
            let behavior = getTileBehavior tileToUpdate.tile
            behavior.update context { position = intent.position }

        | MovePlayerIntent intent ->
            let playerId = context.map.getPlayerId intent.name
            let playerPos, _ = context.map.getEntity playerId
            let rotateEvent = PlayerRotatedEvent { name = intent.name; orientation = intent.direction }
            let subIntent = context.HandleIntent (MoveEntityIntent {
                entityId = playerId;
                newPosition = playerPos + intent.direction.asPoint
            })
            match subIntent.intentResult with
            | IntentAccepted -> subIntent.Accept [ rotateEvent ]
            | IntentRejected -> subIntent.Reject [ rotateEvent ]

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
                        entityId = intent.entityId
                        newPosition = intent.newPosition
                    } ]
                | false ->
                    ctx.Accept []

            let updateSource (ctx: IntentContext) =
                ctx.HandleIntent (UpdateTileIntent { position = oldPosition })
            
            let updateTarget (ctx: IntentContext) =
                ctx.HandleIntent (UpdateTileIntent { position = intent.newPosition })

            let updateSourceDependentTiles (ctx: IntentContext) =
                ctx.HandleIntent (UpdateDependentTilesIntent { position = oldPosition })

            let updateTargetDependentTiles (ctx: IntentContext) =
                ctx.HandleIntent (UpdateDependentTilesIntent { position = intent.newPosition })

            context |> clearTarget
                >>= detachFromSource
                >>= attachToTarget
                >>= emitEvent
                >>= updateSource
                >>= updateTarget
                >>= updateSourceDependentTiles
                >>= updateTargetDependentTiles

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