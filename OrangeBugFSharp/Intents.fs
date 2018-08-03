namespace OrangeBug.Game

open OrangeBug
open System

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
        log: string
    }
    static member Empty = { attemptedMoves = []; log = "" }
    static member Log msg = { attemptedMoves = []; log = msg }
    static member Combine t1 t2 = {
        attemptedMoves = t1.attemptedMoves @ t2.attemptedMoves
        log =
            match t1.log, t2.log with
            | "", s -> s
            | s, "" -> s
            | s1, s2 -> String.concat Environment.NewLine [ s1; s2 ]
    }

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

type GameplayBuilder(context) =
    member __.Bind(x, f) =
        match x context.map with
        | AssertTrue x -> f x
        | AssertFalse msg -> Rejected (ErrorTrace.Log msg)
    member __.Return(result: IntentResult) = result
    member __.ReturnFrom(f: IntentContext -> IntentResult) = f context
    member __.ReturnFrom(intent: Intent) = context.doHandleIntent intent context

module Intent =

    let gameplay = GameplayBuilder
    
    let attempt f context =
        match f context with
        | Accepted _ as ok -> ok
        | Rejected trace -> Accepted []
    
    let inline handle intent context =
        context.doHandleIntent intent context

    let inline emit delay duration ev context =
        Accepted [
            {
                event = ev
                time = context.time + delay
                duration = GameTimeSpan duration
            }
        ]
       
    let emitNow = emit (GameTimeSpan 0)

    let inline trace t _ = Rejected t

    let requireRecentEvent expected ctx =
        if ctx.recentEvents |> List.exists (fun ev -> ev.event = expected)
        then Accepted []
        else Rejected (ErrorTrace.Log (sprintf "Expected but didn't find recent event: %O" expected))

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
