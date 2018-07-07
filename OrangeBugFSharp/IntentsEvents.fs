namespace OrangeBug.Game

open OrangeBug

// Intents

type UpdateTileIntent = {
    position: Point
}

type MovePlayerIntent = {
    name: string
    direction: Direction
}

type TransportationMode =
    | Teleport
    | Push of force: int // >1 needed for pushing adjacent entities

type MoveInitiator = Player | System

type MoveEntityIntent = {
    entityId: EntityId
    newPosition: Point
    mode: TransportationMode
    initiator: MoveInitiator
}

type ClearEntityFromTileIntent = {
    entityId: EntityId
    suggestedPushDirection: Direction option
    move: MoveEntityIntent
}

type AttachEntityToTileIntent = {
    position: Point
    entityToAttach: EntityId
    move: MoveEntityIntent
    moveOldPosition: Point
}

type DetachEntityFromTileIntent = {
    position: Point
    move: MoveEntityIntent
    moveOldPosition: Point
}

type Intent =
    | UpdateTileIntent of UpdateTileIntent
    | MovePlayerIntent of MovePlayerIntent
    | MoveEntityIntent of MoveEntityIntent
    | ClearEntityFromTileIntent of ClearEntityFromTileIntent
    | AttachEntityToTileIntent of AttachEntityToTileIntent
    | DetachEntityFromTileIntent of DetachEntityFromTileIntent


// Events
    
type PlayerRotatedEvent = { name: string; entityId: EntityId; player: PlayerEntity; orientation: Direction; }
type EntityMovedEvent = { entityId: EntityId; oldPosition: Point; newPosition: Point }
type ButtonPressedEvent = { position: Point }
type ButtonReleasedEvent = { position: Point }
type GateOpenedEvent = { position: Point; gate: GateTile }
type GateClosedEvent = { position: Point; gate: GateTile }
type BalloonColoredEvent = { entityId: EntityId; inkPosition: Point; color: InkColor }
type BalloonPoppedEvent = { entityId: EntityId; pinPosition: Point }

type Event =
    | PlayerRotatedEvent of PlayerRotatedEvent
    | EntityMovedEvent of EntityMovedEvent
    | ButtonPressedEvent of ButtonPressedEvent
    | ButtonReleasedEvent of ButtonReleasedEvent
    | GateOpenedEvent of GateOpenedEvent
    | GateClosedEvent of GateClosedEvent
    | BalloonColoredEvent of BalloonColoredEvent
    | BalloonPoppedEvent of BalloonPoppedEvent


// Infrastructure
    
type IntentResult = IntentAccepted | IntentRejected

type IntentContext =
    {
        mapState: GameMapState
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

module Intent =
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
