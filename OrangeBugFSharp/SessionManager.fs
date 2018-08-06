namespace OrangeBug.Hosting

open OrangeBug
open OrangeBug.Game

type Signal =
| ReceiveInitialMap of GameMapState * GameTime
| ReceiveEvents of ScheduledEvent list * GameTime
| ReceiveDebugText of string

module SessionManager =
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
    
    let kill id =
        match sessions.TryFind id with
        | None -> ()
        | Some session ->
            session.clock.stop()
            sessions <- sessions.Remove id