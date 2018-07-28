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
    let TickTargetTime = TimeSpan.FromSeconds 0.25

    let create initialMap = {
        map = initialMap
        time = GameTime 0
        scheduledIntents = []
        scheduledEvents = []
        activeEvents = []
    }

    let private isTileLocked p simulation =
        simulation.scheduledEvents
        |> Seq.collect (fun ev -> Gameplay.eventToAffectedPoints ev.event)
        |> Seq.contains p

    let private mapAccessor simulation mapState =
        let accessor = GameMap.accessor mapState
        // Tiles that will be affected by scheduled events in the future are called "locked".
        // We want intents that involve locked tiles to fail, so instead of returning the actual
        // tile we return a LockedTile when querying the map for a locked position.
        // TODO: Do we need a notion of "locked entities" as well?
        {
            getAt = fun p ->
                if isTileLocked p simulation
                then { position = p; tile = LockedTile; entityId = None }
                else accessor.getAt p

            getEntity = accessor.getEntity
            hasEntity = accessor.hasEntity
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
    let processScheduledIntents sim =
        let intentsToProcess, futureIntents =
            sim.scheduledIntents |> List.partition (fun intent -> intent.time <= sim.time)

        let processScheduledIntent scheduledEvents intent =
            let context = Gameplay.createContextWithAccessor sim.map sim.time (mapAccessor sim)
            let result = context.HandleIntent intent.intent
            match result with
            | Rejected _ -> scheduledEvents
            | Accepted events -> scheduledEvents @ events

        let newEvents = intentsToProcess |> Seq.fold processScheduledIntent sim.scheduledEvents
        { sim with scheduledEvents = newEvents; scheduledIntents = futureIntents }

    /// <summary>
    /// Consumes 'scheduledEvents', updates 'map' and
    /// produces 'activeEvents' and 'scheduledIntents' (via IntentScheduledEvent)
    /// </summary>
    let processScheduledEvents sim =
        let eventsToApply, futureEvents = 
            sim.scheduledEvents 
            |> List.partition (fun ev -> ev.time <= sim.time)

        let applyScheduledEvent (map, scheduledIntents) ev =
            let newMap = Effect.eventToEffects ev.event |> Seq.fold GameMap.applyEffect map
            let newIntents =
                match ev.event with
                | IntentScheduledEvent ev -> { intent = ev.intent :?> Intent; time = ev.time } :: scheduledIntents
                | _ -> scheduledIntents
            newMap, newIntents

        let newMap, newIntents = eventsToApply |> Seq.fold applyScheduledEvent (sim.map, sim.scheduledIntents)

        eventsToApply, {
            sim with
                map = newMap
                scheduledEvents = futureEvents
                scheduledIntents = newIntents
                activeEvents = sim.activeEvents @ eventsToApply
        }

    /// <summary>
    /// Consumes 'activeEvents' producing 'scheduledEvents' (tile updates).
    /// If no event is active, 'time' is advanced by 1 tick.
    /// </summary>
    let processActiveEvents sim =
        let eventsToRemove, remainingEvents =
            sim.activeEvents
            |> List.partition (fun ev -> ev.time.value + ev.duration.value <= sim.time.value)

        match eventsToRemove with
        | [] -> { sim with time = sim.time + (GameTimeSpan 1) }
        | _ ->
            // Schedule tile updates for affected tiles
            let updateEvents =
                eventsToRemove
                |> Seq.collect (fun ev -> Gameplay.eventToAffectedPoints ev.event)
                |> Set.ofSeq
                |> Seq.map (fun p ->
                    {
                        time = sim.time
                        duration = GameTimeSpan 0
                        event = IntentScheduledEvent {
                            intent = UpdateTileIntent { position = p }
                            time = sim.time
                        }
                    })
                |> List.ofSeq

            { sim with activeEvents = remainingEvents; scheduledEvents = sim.scheduledEvents @ updateEvents }
    
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
            let newEvents, newSim =
                sim
                |> processScheduledIntents
                |> processScheduledEvents

            sim <- processActiveEvents newSim
            processedEvents <- processedEvents @ (newEvents |> List.map (fun ev -> ev.event))

        sim, processedEvents

    let private runSimulation initialSimulation (cancellationToken: CancellationToken) (intentQueue: Intent ConcurrentQueue) onSimulationChanged onEvents = async {
        let stopwatch = Stopwatch.StartNew()
        let mutable startTime = TimeSpan.Zero
        let mutable simulation = initialSimulation
        
        while not cancellationToken.IsCancellationRequested do
            // Schedule queued intents
            let mutable intent = ref NopIntent
            while intentQueue.TryDequeue intent do
                simulation <- { simulation with scheduledIntents = { intent = !intent; time = simulation.time } :: simulation.scheduledIntents }

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
    