namespace OrangeBug.Web

module SessionManager =
    open OrangeBug.Game

    type GameSession = {
        clock: SimulationClock
    }

    let mutable sessions = Map.empty<string, GameSession>

    let createSession id initialSimulation onSimulationChanged onEvents =
        match sessions.ContainsKey id with
        | true -> failwithf "SessionManager already has an open session for connection ID '%s'" id
        | false ->
            let newSession = { clock = Simulation.startClock initialSimulation onSimulationChanged onEvents }
            sessions <- sessions |> Map.add id newSession
            newSession
            

    let getSession id = sessions.[id]
    
    let killSession id =
        sessions <- sessions |> Map.remove id