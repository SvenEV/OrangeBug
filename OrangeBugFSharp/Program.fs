// Learn more about F# at http://fsharp.org

namespace OrangeBug

module Program =

    open System
    open System.Text
    open GameMap
    open TilesEntities
    open IntentsEvents

    [<EntryPoint>]
    let main argv =
        let map = GameMap.create 16 10
        map.setAt (Point.create 4 4) (ButtonTile false) None
        map.setAt (Point.create 5 5) (GateTile { isOpen = false; triggerPosition = Point.zero }) None
        map.setAt (Point.create 6 5) (GateTile { isOpen = true; triggerPosition = Point.zero }) None
        map.setAt (Point.create 3 2) PathTile (Some BoxEntity)

        Console.OutputEncoding <- Encoding.UTF8

        while true do
            Console.Clear ()
            Console.WriteLine (Rendering.mapAsString map)
            let key = Console.ReadKey()

            if key.Key = ConsoleKey.Escape then
                Environment.Exit(0)
            
            let direction =
                match key.KeyChar with
                | 'a' -> Some Direction.West
                | 'd' -> Some Direction.East
                | 's' -> Some Direction.South
                | 'w' -> Some Direction.North
                | _ -> None

            match direction with
            | None -> ()
            | Some direction -> 
                let intent = MovePlayerIntent { name = "Player"; direction = direction }
                let result = Gameplay.handleIntent map.accessor intent
                let effects = List.collect (Game.eventToEffects map) result.events
                map.applyEffects effects

            ignore()

        0 // return an integer exit code
