﻿namespace OrangeBug

module IntentsEvents =
    open TilesEntities
    open GameMapTypes

    // Intents

    type UpdateDependentTilesIntent = {
        position: Point
    }

    type UpdateTileIntent = {
        position: Point
    }

    type MovePlayerIntent = {
        name: string
        direction: Direction
    }

    type MoveEntityIntent = {
        entityId: EntityId
        newPosition: Point
    }

    type ClearEntityFromTileIntent = {
        entityId: EntityId
        suggestedPushDirection: Direction option
    }

    type AttachEntityToTileIntent = {
        position: Point
        entityToAttach: EntityId
    }

    type DetachEntityFromTileIntent = {
        position: Point
    }

    type Intent =
        | UpdateDependentTilesIntent of UpdateDependentTilesIntent
        | UpdateTileIntent of UpdateTileIntent
        | MovePlayerIntent of MovePlayerIntent
        | MoveEntityIntent of MoveEntityIntent
        | ClearEntityFromTileIntent of ClearEntityFromTileIntent
        | AttachEntityToTileIntent of AttachEntityToTileIntent
        | DetachEntityFromTileIntent of DetachEntityFromTileIntent


    // Events
    
    type PlayerRotatedEvent = { name: string; orientation: Direction }
    type EntityMovedEvent = { entityId: EntityId; newPosition: Point }
    type BalloonColoredEvent = { entityId: EntityId; inkPosition: Point; color: InkColor }
    type ButtonPressedEvent = { position: Point }
    type ButtonReleasedEvent = { position: Point }
    type GateOpenedEvent = { position: Point }
    type GateClosedEvent = { position: Point }

    type Event =
        | PlayerRotatedEvent of PlayerRotatedEvent
        | EntityMovedEvent of EntityMovedEvent
        | ButtonPressedEvent of ButtonPressedEvent
        | ButtonReleasedEvent of ButtonReleasedEvent
        | GateOpenedEvent of GateOpenedEvent
        | GateClosedEvent of GateClosedEvent
        | BalloonColoredEvent of BalloonColoredEvent
        | BalloonPoppedEvent of EntityId


    // Infrastructure
    
    type IntentResult = IntentAccepted | IntentRejected

    type IntentContext =
        {
            mapState: GameMap
            map: MapAccessor
            emittedEvents: Event list
            intentResult: IntentResult

            doHandleIntent: IntentContext -> Intent -> IntentContext
            acceptIntent: IntentContext -> Event list -> IntentContext
            rejectIntent: IntentContext -> IntentContext
        }
        member this.HandleIntent = this.doHandleIntent this
        member this.Accept = this.acceptIntent this
        member this.Reject = this.rejectIntent this

    let composeIndependent leftHandler rightHandler inContext =
        if inContext.intentResult = IntentRejected then
            failwithf "composeIndependent (=||=>) failed: Incoming context must not be rejected"

        let leftResult = leftHandler inContext

        match leftResult.intentResult with
        | IntentRejected ->
            // if left failed, discard its changes & handle right with previous context
            let rightResult = rightHandler inContext
            match rightResult.intentResult with
            | IntentRejected -> inContext
            | IntentAccepted -> rightResult
        | IntentAccepted ->
            // if left succeeded, use its result as input for the next intent
            let rightResult = rightHandler leftResult
            match rightResult.intentResult with
            | IntentRejected -> leftResult
            | IntentAccepted -> rightResult
    
    let composeDependent leftHandler rightHandler inContext =
        match inContext.intentResult with
        | IntentRejected -> inContext
        | IntentAccepted ->
            let leftResult = leftHandler inContext
            match leftResult.intentResult with
            | IntentRejected -> leftResult // if left intent failed, don't handle right intent
            | IntentAccepted -> rightHandler leftResult

    let (=||=>) a b = composeIndependent a b
    let (=&&=>) a b = composeDependent a b

    