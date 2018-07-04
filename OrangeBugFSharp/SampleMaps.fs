namespace OrangeBug

module SampleMaps =

    open TilesEntities
    open GameMapTypes
    open GameMap

    let createInitialMap() =
        GameMap.Create 12 8
        |> updateTile (Point.create 4 4) (ButtonTile false)
        |> updateTile (Point.create 5 4) (GateTile { isOpen = true; triggerPosition = Point.create 4 4 })
        |> updateTile (Point.create 5 5) (GateTile { isOpen = false; triggerPosition = Point.create 4 4 })
        |> spawnEntity (Point.create 3 2) EntityId.create BoxEntity
        |> spawnEntity (Point.create 4 2) EntityId.create (BalloonEntity Blue)
        |> updateTile (Point.create 6 2) (InkTile Red)
        |> updateTile (Point.create 7 2) (InkTile Green)
        |> updateTile (Point.create 8 2) (PinTile Red)
        |> updateTile (Point.create 8 3) (PinTile Green)
        |> updateTile (Point.create 10 1) (TeleporterTile (Point.create 10 6))
        |> updateTile (Point.create 10 6) (TeleporterTile (Point.create 10 2))