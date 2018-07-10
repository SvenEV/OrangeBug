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


// Infrastructure

type ErrorTrace =
    {
        attemptedMoves: (Point * Point) list
    }
    static member Empty = { attemptedMoves = [] } 
    static member Combine t1 t2 = { attemptedMoves = t1.attemptedMoves @ t2.attemptedMoves }

type IntentResult =
    | Accepted of Event list
    | Rejected of ErrorTrace

type IntentContext =
    {
        mapState: GameMapState
        map: MapAccessor
        
        prevResult: IntentResult
        //emittedEvents: Event list
        recentEvents: Event list

        doHandleIntent: Intent -> IntentContext -> IntentResult

        gameMapApplyEffect: GameMapState -> Effect -> GameMapState
        gameMapCreateAccessor: GameMapState -> MapAccessor
    }
    member this.HandleIntent = fun intent -> this.doHandleIntent intent this

module Intent =

    let emit ev _ = Accepted [ ev ]

    let trace t _ = Rejected t

    let applyEvents context events =
        match context.prevResult with
        | Rejected _ -> failwith "Cannot apply events to rejected IntentContext"
        | Accepted oldEvents ->
            let newMap = events |> Seq.collect Effect.eventToEffects |> Seq.fold context.gameMapApplyEffect context.mapState
            {
                context with 
                    mapState = newMap
                    map = context.gameMapCreateAccessor newMap
                    prevResult = Accepted (oldEvents @ events)
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
