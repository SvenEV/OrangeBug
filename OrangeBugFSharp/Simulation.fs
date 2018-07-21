namespace OrangeBug.Game

open OrangeBug
open System.Threading

type ScheduledEvent = {
    event: Event
    time: GameTime
}

type Simulation = {
    map: GameMapState
    time: GameTime
    scheduledEvents: ScheduledEvent list // not sorted!
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
    let TickTargetTime = TimeSpan.FromSeconds 1.0

    let create initialMap = {
        map = initialMap
        time = GameTime 0
        scheduledEvents = []
    }

    let private eventDuration ev =
        GameTimeSpan (match ev with
        | EntityMovedEvent _ -> 1
        | PlayerRotatedEvent _ -> 0
        | _ -> 0)
    
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
    /// Processes an intent and schedules the resulting events. Time doesn't pass, the map doesn't change.
    /// </summary>
    let processIntent intent simulation =
        let context = Gameplay.IntentContext.createWithAccessor simulation.map (mapAccessor simulation)
        let result = context.HandleIntent intent

        match result with
        | Rejected _ -> simulation
        | Accepted events ->
            // schedule the new events. TODO: We need ways to control whether events should be running sequentially or concurrently
            let newScheduledEvents, _ =
                (GameTimeSpan 0, events)
                ||> Seq.mapFold (fun timeOffset ev -> { event = ev; time = simulation.time + timeOffset }, timeOffset + eventDuration ev)
            {
                simulation with
                    scheduledEvents = simulation.scheduledEvents
                    |> Seq.append newScheduledEvents
                    |> List.ofSeq
            }
    
    /// <summary>
    /// Processes all events scheduled for the current time and then increases the time by 1 tick.
    /// </summary>
    let advance simulation =
        // Processes all events scheduled for the current simulation time and runs tile updates
        // for all affected tiles. Tile updates may in turn schedule new events, even for the same
        // time, hence multiple simulation passes may be necessary to advance the time by 1 tick.
        // This terminates as soon as there are no more events to process in a simulation pass.
        let runSinglePass sim =
            let eventsToApply, futureEvents = 
                sim.scheduledEvents 
                |> List.partition (fun ev -> ev.time.value <= sim.time.value)

            match eventsToApply.Length with
            | 0 ->
                // No events scheduled for now, we can advance the time
                [], { sim with time = sim.time + (GameTimeSpan 1) }

            | _ ->
                // Apply pending events
                let applyScheduledEvent (map, affectedPoints) ev =
                    let newAffectedPoints = Set.union affectedPoints (Gameplay.eventToAffectedPoints ev.event |> Set.ofSeq)
                    let newMap = Effect.eventToEffects ev.event |> Seq.fold GameMap.applyEffect map
                    newMap, newAffectedPoints

                let newMap, affectedPoints = Seq.fold applyScheduledEvent (sim.map, Set.empty) eventsToApply

                // Try to update affected tiles.
                // Fails for locked tiles, but that's ok as those will be updated anyway once they are unlocked.
                // Updates may produce new events that are added to the schedule.

                // Idea: tile updates cannot run independently, i.e. looking at the same map, as that might cause
                // incompatible/overlapping events. Update events should be concurrent though.
                // But we probably want to delay consecutive tile updates (caused by dependencies) by 1 tick.
                // This does NOT solve the problem of dependency cycles, though - we'd get a stack overflow.

                let updateSim = { sim with map = newMap; scheduledEvents = futureEvents }
                let updateContext = Gameplay.IntentContext.createWithAccessor newMap (mapAccessor updateSim)
                let updateEvents =
                    Gameplay.updateTiles affectedPoints updateContext
                    |> List.map (fun ev -> { event = ev; time = sim.time }) // TODO: zero delay for now

                eventsToApply, {
                    map = newMap
                    time = updateSim.time // time doesn't advance as long as there are events to process
                    scheduledEvents = updateEvents @ futureEvents
                }

        // simulate single passes until time has been advanced by one tick
        let mutable sim = simulation
        let mutable processedEvents = []

        while sim.time = simulation.time do
            let newEvents, newSim = runSinglePass sim
            processedEvents <- processedEvents @ (newEvents |> List.map (fun ev -> ev.event))
            sim <- newSim

        sim, processedEvents

    let private runSimulation initialSimulation (cancellationToken: CancellationToken) (intentQueue: Intent ConcurrentQueue) onSimulationChanged onEvents = async {
        let stopwatch = Stopwatch.StartNew()
        let mutable startTime = TimeSpan.Zero
        let mutable simulation = initialSimulation
        
        while not cancellationToken.IsCancellationRequested do
            let mutable intent = NopIntent
            while intentQueue.TryDequeue &intent do
                simulation <- processIntent intent simulation
                onSimulationChanged simulation

            let newSim, events = advance simulation
            simulation <- newSim
            onSimulationChanged newSim
            if not events.IsEmpty then onEvents (events, newSim.time)

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
    