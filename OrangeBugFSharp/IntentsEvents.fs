namespace OrangeBug

module IntentsEvents =

    // Intents
    type MovePlayerIntent = {
        name: string
        direction: Direction
    }

    type MoveEntityIntent =
        {
            sourcePosition: Point
            targetPosition: Point
        }
        member this.offset = this.targetPosition - this.sourcePosition

    type ClearEntityFromTileIntent = {
        position: Point
        suggestedPushDirection: Direction option
    }

    type Intent =
        | MovePlayerIntent of MovePlayerIntent
        | MoveEntityIntent of MoveEntityIntent
        | ClearEntityFromTileIntent of ClearEntityFromTileIntent
        | DetachEntityFromTileIntent
        | AttachEntityToTileIntent

    // Events
    type PlayerRotatedEvent = { name: string; orientation: Direction }
    type EntityMovedEvent = { sourcePosition: Point; targetPosition: Point }
    type BalloonColoredEvent = { position: Point; color: InkColor }

    type Event =
        | PlayerRotatedEvent of PlayerRotatedEvent
        | EntityMovedEvent of EntityMovedEvent
        | ButtonPressedEvent
        | ButtonReleasedEvent
        | BalloonColoredEvent of BalloonColoredEvent
        | BalloonPoppedEvent of Point
    
    // Helpers
    type IntentResult =
        | IntentAccepted of Event list
        | IntentRejected of Event list
        member this.events =
            match this with
            | IntentAccepted events -> events
            | IntentRejected events -> events
    
    let bind handleNextIntent prevResult =
        match prevResult with
        // if previous intent failed, do not handle next intent (fail early)
        | IntentRejected _ as rejection -> rejection
        // if previous intent succeeded, handle next and return events from both intents
        | IntentAccepted events ->
            match handleNextIntent events with
            | IntentAccepted moreEvents -> IntentAccepted (events @ moreEvents)
            | IntentRejected moreEvents -> IntentRejected (events @ moreEvents)
    
    let (>>=) prevResult handleNextIntent = bind handleNextIntent prevResult