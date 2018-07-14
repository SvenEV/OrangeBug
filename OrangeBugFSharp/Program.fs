﻿namespace OrangeBug

module Program =

    open System
    open System.Text
    open OrangeBug.Game
    open OrangeBug.Grid

    let runGame() =
        let mutable map = SampleMaps.createInitialMap()

        Console.OutputEncoding <- Encoding.UTF8

        while true do
            Console.Clear ()
            Console.WriteLine (Rendering.mapAsString map)
            Console.WriteLine (map.tiles |> Grid.asCharBitmap |> CharBitmap.toString)
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
                match Gameplay.processIntent intent map with
                | Rejected trace -> printfn "INTENT FAILED! Trace: %O" trace
                | Accepted events -> map <- events |> Seq.collect Effect.eventToEffects |> Seq.fold GameMap.applyEffect map
            

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