namespace OrangeBug.Game

open OrangeBug
open System.Threading

type ScheduledIntent = {
    intent: Intent
    time: GameTime
}

type Simulation = {
    map: GameMapState
    time: GameTime
    scheduledIntents: ScheduledIntent list
    scheduledEvents: ScheduledEvent list
    activeEvents: ScheduledEvent list
}

type SimulationClock = {
    stop: unit -> unit
    queueIntent: Intent -> unit
}

module Simulation =
    open System.Diagnostics
    open System
    open System.Collections.Concurrent

    // 1 / TickTargetTime = frames per second
    let TickTargetTime = TimeSpan.FromSeconds 0.125

    let create initialMap = {
        map = initialMap
        time = GameTime 0
        scheduledIntents = []
        scheduledEvents = []
        activeEvents = []
    }

    let private isTileLocked p simulation =
        simulation.scheduledEvents
        |> Seq.append simulation.activeEvents
        |> Seq.collect (fun ev -> Gameplay.eventToAffectedPoints ev.event)
        |> Seq.contains p

    let private mapAccessor simulation mapState =
        let accessor = GameMap.accessor mapState
        // Tiles that will be affected by scheduled events in the future are called "locked".
        // We want intents that involve locked tiles to fail, so instead of returning the actual
        // tile we return a LockedTile when querying the map for a locked position.
        // TODO: Do we need a notion of "locked entities" as well?
        {
            getTile = fun p ->
                if isTileLocked p simulation
                then LockedTile, None
                else accessor.getTile p

            tryGetEntity = accessor.tryGetEntity
            getPlayerId = accessor.getPlayerId
            
            getDependenciesOf = fun p ->
                if isTileLocked p simulation
                then Set.empty
                else accessor.getDependenciesOf p

            getPositionsDependentOn = accessor.getPositionsDependentOn
        }
    
    /// <summary>
    /// Consumes 'scheduledIntents' producing 'scheduledEvents'
    /// </summary>
    let processScheduledIntents simulation =
        let intentsToProcess, futureIntents =
            simulation.scheduledIntents
            |> List.partition (fun intent -> intent.time <= simulation.time)

        let processScheduledIntent sim intent =
            let context = Gameplay.createContextWithAccessor sim.map sim.time (mapAccessor sim)
            let result = context |> Intent.handle intent.intent
            match result with
            | Rejected trace ->
                printfn "Failed to process intent '%O': %s" intent trace.log 
                sim
            | Accepted events -> { sim with scheduledEvents = sim.scheduledEvents @ events }

        let simAfterIntents = intentsToProcess |> Seq.fold processScheduledIntent simulation
        { simAfterIntents with scheduledIntents = futureIntents }

    /// <summary>
    /// Consumes 'scheduledEvents', updates 'map' and
    /// produces 'activeEvents' and 'scheduledIntents' (via IntentScheduledEvent).
    /// </summary>
    let processScheduledEvents simulation =
        let eventsToApply, futureEvents = 
            simulation.scheduledEvents 
            |> List.partition (fun ev -> ev.time <= simulation.time)

        let applyScheduledEvent (map, scheduledIntents) ev =
            let newMap = Effect.eventToEffects ev.event |> Seq.fold GameMap.applyEffect map
            let newIntents =
                match ev.event with
                | IntentScheduledEvent ev -> { intent = ev.intent :?> Intent; time = ev.time } :: scheduledIntents
                | _ -> scheduledIntents
            newMap, newIntents

        let newMap, newIntents = eventsToApply |> Seq.fold applyScheduledEvent (simulation.map, simulation.scheduledIntents)

        eventsToApply, {
            simulation with
                map = newMap
                scheduledEvents = futureEvents
                scheduledIntents = newIntents
                activeEvents = simulation.activeEvents @ eventsToApply
        }

    /// <summary>
    /// Consumes 'activeEvents' producing 'scheduledEvents' (tile updates).
    /// </summary>
    let processActiveEvents simulation =
        let eventsToRemove, remainingEvents =
            simulation.activeEvents
            |> List.partition (fun ev -> ev.time.value + ev.duration.value <= simulation.time.value)

        // Schedule tile updates for affected tiles
        let updateEvents =
            eventsToRemove
            |> Seq.collect (fun ev -> Gameplay.eventToAffectedPoints ev.event)
            |> Set.ofSeq
            |> Seq.map (fun p ->
                {
                    time = simulation.time
                    duration = GameTimeSpan 0
                    event = IntentScheduledEvent {
                        intent = UpdateTileIntent { position = p }
                        time = simulation.time
                    }
                })
            |> List.ofSeq

        { simulation with activeEvents = remainingEvents; scheduledEvents = simulation.scheduledEvents @ updateEvents }
    
    /// <summary>
    /// Processes all intents and events scheduled for the current time and then increases the time by 1 tick.
    /// Returns a list of events that were applied.
    /// </summary>
    let advance simulation =
        // Repeat until no more 'activeEvents' are completed, hence time is advanced:
        // 1. Process scheduled intents, resulting in new events being scheduled.
        // 2. Apply scheduled events to map and move them into 'activeEvents'.
        //    'IntentScheduledEvent'-s cause new intents to be scheduled.
        // 3. Remove completed events from 'activeEvents' and schedule tile updates
        //    for all directly affected tiles.
        let mutable sim = simulation
        let mutable processedEvents = []

        while sim.time = simulation.time do
            let newEvents, newSim = processScheduledEvents sim
            
            let newSim =
                newSim
                |> processActiveEvents
                |> processScheduledIntents

            sim <-
                if newSim.scheduledEvents |> Seq.exists (fun ev -> ev.time <= newSim.time)
                then newSim // continue simulating for current time
                else { newSim with time = newSim.time + (GameTimeSpan 1) }
            
            processedEvents <- processedEvents @ newEvents

        sim, processedEvents

    let private runSimulation initialSimulation (cancellationToken: CancellationToken) (intentQueue: Intent ConcurrentQueue) onSimulationChanged onEvents = async {
        let stopwatch = Stopwatch.StartNew()
        let mutable startTime = TimeSpan.Zero
        let mutable simulation = initialSimulation
        
        while not cancellationToken.IsCancellationRequested do
            // Schedule queued intents
            let mutable intent = ref NopIntent
            while intentQueue.TryDequeue intent do
                simulation <- {
                    simulation with
                        scheduledIntents = simulation.scheduledIntents @ [ { intent = !intent; time = simulation.time } ]
                }

            // Advance simulation
            let newSim, events = advance simulation
            simulation <- newSim
            onSimulationChanged newSim
            if not events.IsEmpty then onEvents (events, newSim.time)

            // Wait until target tick time reached
            let deltaTime = stopwatch.Elapsed - startTime
            let waitTime = int (TickTargetTime - deltaTime).TotalMilliseconds
            if waitTime > 0 then do! Async.Sleep waitTime
            startTime <- stopwatch.Elapsed
    }

    let startClock initialSimulation onSimulationChanged onEvents =
        let cancellationTokenSource = new CancellationTokenSource()
        let intentQueue = new ConcurrentQueue<Intent>()
        runSimulation initialSimulation cancellationTokenSource.Token intentQueue onSimulationChanged onEvents |> Async.Start
        {
            stop = fun () -> cancellationTokenSource.Cancel()
            queueIntent = intentQueue.Enqueue
        }
    