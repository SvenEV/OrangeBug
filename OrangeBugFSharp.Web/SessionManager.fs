namespace OrangeBug.Web

module SessionManager =
    open OrangeBug.GameMap
    open OrangeBug
    open GameMapTypes
    open TilesEntities

    type GameSession = {
        mutable map: GameMap
    }

    let createInitialMap() =
        GameMap.create 8 6
        |> updateTile (Point.create 4 4) (ButtonTile false)
        |> updateTile (Point.create 5 5) (GateTile { isOpen = false; triggerPosition = Point.create 4 4 })
        |> updateTile (Point.create 6 5) (GateTile { isOpen = true; triggerPosition = Point.create 4 4 })
        |> spawnEntity (Point.create 3 2) BoxEntity

    let mutable sessions = Map.empty<string, GameSession>

    let getOrCreateSession id =
        match sessions.TryFind id with
        | Some session -> session
        | None ->
            let newSession = { map = createInitialMap() }
            sessions <- sessions |> Map.add id newSession
            newSession
            

    let killSession id =
        sessions <- sessions |> Map.remove id