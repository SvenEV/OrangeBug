namespace OrangeBug

module Program =

    open System
    open System.Text
    open OrangeBug.Game
    open OrangeBug.Grid

    let runGame() =
        let mutable simulation = Simulation.create (SampleMaps.createInitialMap())

        Console.OutputEncoding <- Encoding.UTF8

        while true do
            Console.Clear ()
            Console.WriteLine (sprintf "Time: %i - %i events scheduled (up to time %i)" simulation.time.value simulation.scheduledEvents.Length (if simulation.scheduledEvents.Length = 0 then -1 else (Seq.maxBy (fun (ev: ScheduledEvent) -> ev.time.value) simulation.scheduledEvents).time.value))
            Console.WriteLine (Rendering.mapAsString simulation.map)
            //Console.WriteLine (simulation.map.tiles |> Grid.asCharBitmap |> CharBitmap.toString)
            let key = Console.ReadKey()

            match key.Key with
            | ConsoleKey.Escape -> Environment.Exit(0)
            | ConsoleKey.Enter ->
                // advance simulation by 1 tick
                let newSim, events = Simulation.advance simulation
                simulation <- newSim
            | _ ->
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
                    // schedule player move
                    let intent = {
                        time = simulation.time
                        intent = MovePlayerIntent { name = "Player"; direction = direction }
                    }
                    simulation <- { simulation with scheduledIntents = intent :: simulation.scheduledIntents }
                    // advance simulation by 1 tick
                    let newSim, _ = Simulation.advance simulation
                    simulation <- newSim

            
    let testGrid() =
        let printGrid grid = Grid.asCharBitmap grid |> CharBitmap.toString |> printfn "%s\n"

        let grid = Grid.empty (Point.create 10 10)
        
        let grid = grid |> Grid.add (Point.create 0 0) (ButtonTile { isPressed = false })
        printGrid grid
        let grid = grid |> Grid.add (Point.create 8 2) WallTile
        printGrid grid
        let grid = grid |> Grid.add (Point.create 9 2) WallTile
        printGrid grid
        let grid2 = grid |> Grid.add (Point.create 8 2) (ButtonTile { isPressed = true })
        printGrid grid2

        let oldResult = grid |> Grid.tryFind (Point.create 8 2)
        let newResult = grid2 |> Grid.tryFind (Point.create 8 2)

        let grid = grid |> Grid.remove (Point.create 8 2)
        printGrid grid
        let grid = grid |> Grid.remove (Point.create 9 2)
        printGrid grid
        let grid = grid |> Grid.remove (Point.create 0 0)
        printGrid grid

    [<EntryPoint>]
    let main argv =
        runGame()
        0