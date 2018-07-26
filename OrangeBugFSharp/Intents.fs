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

type MoveInitiator = Other | SomeTeleporter | SomePiston

type MoveEntityIntent = {
    entityId: EntityId
    newPosition: Point
    mode: TransportationMode
    initiator: MoveInitiator
    duration: GameTimeSpan
}

type ClearEntityFromTileIntent = {
    entityId: EntityId
    suggestedPushDirection: Direction option
    move: MoveEntityIntent
}

type AttachEntityToTileIntent = {
    oldPosition: Point
    move: MoveEntityIntent
}

type DetachEntityFromTileIntent = {
    position: Point
    move: MoveEntityIntent
}

type Intent =
    | UpdateTileIntent of UpdateTileIntent
    | MovePlayerIntent of MovePlayerIntent
    | MoveEntityIntent of MoveEntityIntent
    | ClearEntityFromTileIntent of ClearEntityFromTileIntent
    | AttachEntityToTileIntent of AttachEntityToTileIntent
    | DetachEntityFromTileIntent of DetachEntityFromTileIntent
    | NopIntent


// Infrastructure

type ErrorTrace =
    {
        attemptedMoves: (Point * Point) list
    }
    static member Empty = { attemptedMoves = [] } 
    static member Combine t1 t2 = { attemptedMoves = t1.attemptedMoves @ t2.attemptedMoves }

type IntentResult =
    | Accepted of ScheduledEvent list
    | Rejected of ErrorTrace

type IntentContext =
    {
        mapState: GameMapState
        map: MapAccessor

        events: ScheduledEvent list
        recentEvents: ScheduledEvent list
        time: GameTime

        doHandleIntent: Intent -> IntentContext -> IntentResult
        gameMapApplyEffect: GameMapState -> Effect -> GameMapState
        gameMapCreateAccessor: GameMapState -> MapAccessor
    }
    member this.HandleIntent = fun intent -> this.doHandleIntent intent this

module Intent =

    let emit delay duration ev (context: IntentContext) =
        Accepted [
            {
                event = ev
                time = context.time + delay
                duration = GameTimeSpan duration
            }
        ]
       
    let emitNow = emit (GameTimeSpan 0)

    let trace t _ = Rejected t

    let applyEvents context events =
        let newMap =
            events 
            |> Seq.map (fun ev -> ev.event) 
            |> Seq.collect Effect.eventToEffects
            |> Seq.fold context.gameMapApplyEffect context.mapState
        let allEvents = context.events @ events
        {
            context with 
                mapState = newMap
                map = context.gameMapCreateAccessor newMap
                events = allEvents
                recentEvents = events
        }

    let private composeIndependent leftHandler rightHandler inContext =
        let leftResult = leftHandler inContext
        match leftResult with
        | Rejected trace ->
            // if left failed, discard its changes & handle right with previous context
            match rightHandler inContext with
            | Accepted _ as rightResult -> rightResult
            | Rejected trace2 -> Rejected (ErrorTrace.Combine trace trace2)
        | Accepted leftEvents ->
            // if left succeeded, use its result as input for the next intent
            let newContext = applyEvents inContext leftEvents
            let rightResult = rightHandler newContext
            match rightResult with
            | Rejected _ -> leftResult
            | Accepted rightEvents -> Accepted (leftEvents @ rightEvents)
    
    let private composeDependent leftHandler rightHandler inContext =
        let leftResult = leftHandler inContext
        match leftResult with
        | Rejected _ -> leftResult // if left intent failed, don't handle right intent
        | Accepted leftEvents ->
            let newContext = applyEvents inContext leftEvents
            let rightResult = rightHandler newContext
            match rightResult with
            | Rejected _ -> rightResult
            | Accepted rightEvents -> Accepted (leftEvents @ rightEvents)

    let (=||=>) = composeIndependent
    let (=&&=>) = composeDependent
