namespace OrangeBug.Web

module SessionManager =
    open OrangeBug.Game

    type GameSession = {
        mutable simulation: Simulation
    }

    let mutable sessions = Map.empty<string, GameSession>

    let getOrCreateSession id =
        match sessions.TryFind id with
        | Some session -> session
        | None ->
            let newSession = { simulation = Simulation.create (SampleMaps.createInitialMap()) }
            sessions <- sessions |> Map.add id newSession
            newSession
            

    let killSession id =
        sessions <- sessions |> Map.remove id