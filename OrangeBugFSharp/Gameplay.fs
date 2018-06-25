namespace OrangeBug

module Gameplay =
    open IntentsEvents
    open Behaviors
    
    let rec handleIntent (map: MapAccessor) intent =
        match intent with
        | MovePlayerIntent intent ->
            let playerPos = map.getPlayerPosition intent.name
            let rotateEvent = PlayerRotatedEvent { name = intent.name; orientation = intent.direction }
            let subIntent = handleIntent map (MoveEntityIntent {
                sourcePosition = playerPos;
                targetPosition = playerPos + intent.direction.asPoint
            })
            match subIntent with
            | IntentAccepted evs -> IntentAccepted (rotateEvent :: evs)
            | IntentRejected evs -> IntentRejected (rotateEvent :: evs)

        | MoveEntityIntent intent ->
            let source = map.getAt intent.sourcePosition
            let target = map.getAt intent.targetPosition

            let clearTarget _ =
                match target.entity with
                | None -> IntentAccepted [] // no entity to remove
                | Some entity ->
                    let behavior = entity |> Behaviors.getEntityBehavior
                    behavior.tryClearTile {
                        tileInfo = target;
                        map = map
                        suggestedPushDirection = intent.offset.asDirection
                    }
            
            let detachFromSource _ =
                let behavior = source.tile |> Behaviors.getTileBehavior
                behavior.tryDetachEntity { tileInfo = source; map = map }

            let attachToTarget _ =
                let behavior = target.tile |> Behaviors.getTileBehavior
                behavior.tryAttachEntity {
                    tileInfo = target
                    map = map
                    entityToAttach = source.entity.Value
                }

            let emitEvent _ =
                IntentAccepted (EntityMovedEvent { 
                    sourcePosition = intent.sourcePosition
                    targetPosition = intent.targetPosition
                } :: [])

            [] |> clearTarget >>= detachFromSource >>= attachToTarget >>= emitEvent

        | _ -> IntentRejected []

