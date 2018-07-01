namespace OrangeBug

module Gameplay =
    open IntentsEvents
    open Behaviors

    let rec handleIntent context intent =
        match intent with
        | UpdateDependentTilesIntent intent ->
            let handleDependency ctx pos =
                let (updateResult: IntentContext) = handleIntent ctx (UpdateAfterDependencyChangedIntent {
                    position = pos;
                    dependencyPosition = intent.position
                })
                // Failing updates should not fail the whole intent chain
                updateResult.accept []

            let dependencies = context.map.getPositionsDependentOn intent.position
            dependencies |> Set.fold handleDependency (context.accept [])

        | UpdateAfterDependencyChangedIntent intent ->
            let tileToUpdate = context.map.getAt intent.position
            let behavior = getTileBehavior tileToUpdate.tile
            behavior.update context {
                position = intent.position
                dependencyPosition = intent.dependencyPosition
            }

        | MovePlayerIntent intent ->
            let playerId = context.map.getPlayerId intent.name
            let playerPos, _ = context.map.getEntity playerId
            let rotateEvent = PlayerRotatedEvent { name = intent.name; orientation = intent.direction }
            let subIntent = handleIntent context (MoveEntityIntent {
                entityId = playerId;
                newPosition = playerPos + intent.direction.asPoint
            })
            match subIntent.intentResult with
            | IntentAccepted -> subIntent.accept [ rotateEvent ]
            | IntentRejected -> subIntent.reject [ rotateEvent ]

        | MoveEntityIntent intent ->
            let oldPosition, entity = context.map.getEntity intent.entityId
            let target = context.map.getAt intent.newPosition
            let offset = intent.newPosition - oldPosition

            let clearTarget ctx =
                match target.entityId with
                | Some id ->
                    handleIntent ctx (ClearEntityFromTileIntent { 
                        entityId = id
                        suggestedPushDirection = offset.asDirection
                    })
                | None -> ctx.accept []
            
            let detachFromSource ctx =
                handleIntent ctx (DetachEntityFromTileIntent { position = oldPosition })

            let attachToTarget ctx =
                handleIntent ctx (AttachEntityToTileIntent {
                    position = intent.newPosition
                    entityToAttach = entity
                })

            let emitEvent (ctx: IntentContext) =
                ctx.accept (EntityMovedEvent { 
                    entityId = intent.entityId
                    newPosition = intent.newPosition
                } :: [])

            let updateSourceDependentTiles map =
                handleIntent map (UpdateDependentTilesIntent { position = oldPosition })

            let updateTargetDependentTiles map =
                handleIntent map (UpdateDependentTilesIntent { position = intent.newPosition })

            context |> clearTarget
                >>= detachFromSource 
                >>= attachToTarget 
                >>= emitEvent
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
