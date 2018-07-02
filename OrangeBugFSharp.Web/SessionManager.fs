namespace OrangeBug.Web

module SessionManager =
    open OrangeBug
    open GameMapTypes

    type GameSession = {
        mutable map: GameMap
    }

    let mutable sessions = Map.empty<string, GameSession>

    let getOrCreateSession id =
        match sessions.TryFind id with
        | Some session -> session
        | None ->
            let newSession = { map = SampleMaps.createInitialMap() }
            sessions <- sessions |> Map.add id newSession
            newSession
            

    let killSession id =
        sessions <- sessions |> Map.remove id