namespace OrangeBug

module IntentsEvents =
    open TilesEntities
    open GameMapTypes

    // Intents

    type UpdateDependentTilesIntent = {
        position: Point
    }

    type UpdateAfterDependencyChangedIntent = {
        position: Point
        dependencyPosition: Point
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
        entityToAttach: Entity
    }

    type DetachEntityFromTileIntent = {
        position: Point
    }

    type Intent =
        | UpdateDependentTilesIntent of UpdateDependentTilesIntent
        | UpdateAfterDependencyChangedIntent of UpdateAfterDependencyChangedIntent
        | MovePlayerIntent of MovePlayerIntent
        | MoveEntityIntent of MoveEntityIntent
        | ClearEntityFromTileIntent of ClearEntityFromTileIntent
        | AttachEntityToTileIntent of AttachEntityToTileIntent
        | DetachEntityFromTileIntent of DetachEntityFromTileIntent


    // Events
    
    type PlayerRotatedEvent = { name: string; orientation: Direction }
    type EntityMovedEvent = { entityId: EntityId; newPosition: Point }
    type BalloonColoredEvent = { entityId: EntityId; color: InkColor }
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
            rejectIntent: IntentContext -> Event list -> IntentContext
        }
        member this.handleIntent = this.doHandleIntent this
        member this.accept = this.acceptIntent this
        member this.reject = this.rejectIntent this

    let bind handleNextIntent prevResult =
        match prevResult.intentResult with
        | IntentRejected -> prevResult // if previous intent failed, don't handle next intent (fail early)
        | IntentAccepted -> handleNextIntent prevResult
    
    let (>>=) prevResult handleNextIntent = bind handleNextIntent prevResult