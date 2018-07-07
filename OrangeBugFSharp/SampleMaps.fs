namespace OrangeBug.Game

module SampleMaps =

    open OrangeBug

    let createInitialMap() =
        GameMap.create 12 8
        |> GameMap.updateTile (Point.create 4 4) (ButtonTile false)
        |> GameMap.updateTile (Point.create 5 4) (GateTile { isOpen = true; triggerPosition = Point.create 4 4 })
        |> GameMap.updateTile (Point.create 5 5) (GateTile { isOpen = false; triggerPosition = Point.create 4 4 })
        |> GameMap.spawnEntity (Point.create 3 2) EntityId.create BoxEntity
        |> GameMap.spawnEntity (Point.create 4 2) EntityId.create (BalloonEntity Blue)
        |> GameMap.updateTile (Point.create 6 2) (InkTile Red)
        |> GameMap.updateTile (Point.create 7 2) (InkTile Green)
        |> GameMap.updateTile (Point.create 8 2) (PinTile Red)
        |> GameMap.updateTile (Point.create 8 3) (PinTile Green)
        |> GameMap.updateTile (Point.create 10 1) (TeleporterTile (Point.create 10 6))
        |> GameMap.updateTile (Point.create 10 6) (TeleporterTile (Point.create 10 2))
        |> GameMap.updateTile (Point.create 2 2) (CornerTile West)
        |> GameMap.updateTile (Point.create 2 4) (CornerTile East)
        |> GameMap.updateTile (Point.create 2 3) (TeleporterTile (Point.create 2 2))
        |> GameMap.updateTile (Point.create 2 1) (TeleporterTile (Point.create 2 2))