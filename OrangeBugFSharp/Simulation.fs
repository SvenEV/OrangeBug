namespace OrangeBug.Game

open OrangeBug

type ScheduledEvent = {
    event: Event
    time: GameTime
}

type Simulation = {
    map: GameMapState
    time: GameTime
    scheduledEvents: ScheduledEvent list // not sorted!
}

module Simulation =

    let create initialMap = {
        map = initialMap
        time = GameTime 0
        scheduledEvents = []
    }

    let private eventDuration ev =
        match ev with
        | EntityMovedEvent _ -> GameTimeSpan 2
        | BalloonPoppedEvent _ -> GameTimeSpan 1
        | _ -> GameTimeSpan 0

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

                // TODO: Incorporate tile dependency graph

                // Idea: tile updates should probably be independent, i.e. all "concurrent" tile updates see the same map
                // regardless of the order how affectedPoints are processed. Update events should be concurrent.
                // But consecutive tile updates (caused by dependencies) must see changes of prior updates and we
                // probably want to delay them by 1 tick to ensure that 'advance' terminates in case of a dependency cycle
                // where two tiles constantly cause each other to update.

                let updateSim = { sim with map = newMap; scheduledEvents = futureEvents }
                let updateContext = Gameplay.IntentContext.createWithAccessor newMap (mapAccessor updateSim)
                let updateTile (ctx: IntentContext) events p =
                    match ctx.HandleIntent (UpdateTileIntent { position = p }) with
                    | Rejected _ -> events // tile locked probably? doesn't matter though
                    | Accepted updateEvents -> events @ updateEvents

                let updateEvents =
                    affectedPoints
                    |> Seq.fold (updateTile updateContext) []
                    |> Seq.map (fun ev -> { event = ev; time = updateSim.time })
                    |> List.ofSeq

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
