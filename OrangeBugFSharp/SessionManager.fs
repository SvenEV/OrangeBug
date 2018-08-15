namespace OrangeBug.Hosting

open OrangeBug
open OrangeBug.Game

type Signal =
| ReceiveInitialMap of map: GameMapState * time: SimTime * tickTargetTime: float
| ReceiveEvents of events: ScheduledEvent list * time: SimTime
| ReceiveDebugText of text: string

type Request =
| RequestIntent of intent: Intent

module SessionManager =
    open System

    type GameSession = {
        id: string
        clock: SimulationClock
        onSignal: Signal -> unit
        mutable simulation: Simulation
    }

    let mutable sessions = Map.empty<string, GameSession>

    let private updateSimulation id newSim =
        match sessions.TryFind id with
        | Some session -> session.simulation <- newSim
        | None -> failwithf "Tried to update simulation of non-existent session '%s'" id

    let private signalEvents id eventsAndTime =
        match sessions.TryFind id with
        | Some session -> session.onSignal (ReceiveEvents eventsAndTime)
        | None -> failwithf "Tried to signal events to non-existent session '%s'" id

    let create onSignal initialSimulation id =
        if sessions.ContainsKey id then
            failwithf "SessionManager already has an open session with ID '%s'" id
        else
            let newSession = {
                id = id
                clock = Simulation.startClock initialSimulation (updateSimulation id) (signalEvents id)
                onSignal = onSignal
                simulation = initialSimulation
            }
            sessions <- sessions |> Map.add id newSession
            onSignal (ReceiveInitialMap (initialSimulation.map, initialSimulation.time, Simulation.TickTargetTime.TotalSeconds))
    
    let handleRequest id request =
        match sessions.TryFind id with
        | None -> failwithf "Tried to handle request for non-existent session '%s" id
        | Some session ->
            match request with
            | RequestIntent intent -> session.clock.queueIntent intent
    
    let kill id =
        match sessions.TryFind id with
        | None -> ()
        | Some session ->
            session.clock.stop()
            sessions <- sessions.Remove id
            Console.Clear()