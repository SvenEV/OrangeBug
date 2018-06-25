namespace OrangeBug.Web

module SessionManager =
    open OrangeBug.GameMap
    open OrangeBug
    open OrangeBug.TilesEntities

    type GameSession = {
        map: GameMap
    }

    let createInitialMap() =
        let map = GameMap.create 14 10
        map.setAt (Point.create 4 4) (ButtonTile false) None
        map.setAt (Point.create 5 5) (GateTile { isOpen = false; triggerPosition = Point.zero }) None
        map.setAt (Point.create 6 5) (GateTile { isOpen = true; triggerPosition = Point.zero }) None
        map.setAt (Point.create 3 2) PathTile (Some BoxEntity)
        map

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