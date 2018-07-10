namespace OrangeBug.Game

module SampleMaps =

    open OrangeBug

    let createInitialMap() =
        GameMap.create 15 8
        |> GameMap.updateTile (Point.create 4 4) (ButtonTile false)
        |> GameMap.updateTile (Point.create 5 4) (GateTile { isOpen = true; triggerPosition = Point.create 4 4 })
        |> GameMap.updateTile (Point.create 5 5) (GateTile { isOpen = false; triggerPosition = Point.create 4 4 })
        |> GameMap.spawnEntity (Point.create 3 2) EntityId.create BoxEntity
        |> GameMap.spawnEntity (Point.create 4 2) EntityId.create (BalloonEntity Blue)
        |> GameMap.updateTile (Point.create 6 2) (InkTile Red)
        |> GameMap.updateTile (Point.create 7 2) (InkTile Green)
        |> GameMap.updateTile (Point.create 8 2) (PinTile Red)
        |> GameMap.updateTile (Point.create 8 3) (PinTile Green)
        |> GameMap.updateTile (Point.create 10 1) (TeleporterTile { targetPosition = Point.create 10 6; isActive = true })
        |> GameMap.updateTile (Point.create 10 6) (TeleporterTile { targetPosition = Point.create 10 2; isActive = true })
        |> GameMap.updateTile (Point.create 2 2) (CornerTile West)
        |> GameMap.updateTile (Point.create 2 4) (CornerTile East)
        |> GameMap.updateTile (Point.create 2 3) (TeleporterTile { targetPosition = Point.create 2 2; isActive = true })
        |> GameMap.updateTile (Point.create 2 1) (TeleporterTile { targetPosition = Point.create 2 2; isActive = true })
        |> GameMap.updateTile (Point.create 12 3) (PistonTile { orientation = North; triggerPosition = Point.create 4 4; force = 2; isExtended = false })
        |> GameMap.spawnEntity (Point.create 12 3) EntityId.create (PistonEntity North)
        |> GameMap.updateTile (Point.create 12 4) (CornerTile South)
        //|> GameMap.updateTile (Point.create 12 4) (TeleporterTile { targetPosition = Point.create 2 6; isActive = true })
        |> GameMap.spawnEntity (Point.create 12 4) EntityId.create BoxEntity