﻿namespace OrangeBug.Game

module SampleMaps =

    open OrangeBug

    let sampleMap1 =
        GameMap.create 20 12
        |> GameMap.updateTile (Point.create 4 4) (ButtonTile { isPressed = false })
        |> GameMap.updateTile (Point.create 5 4) (GateTile { isOpen = true; triggerPosition = Point.create 4 4 })
        |> GameMap.updateTile (Point.create 5 5) (GateTile { isOpen = false; triggerPosition = Point.create 4 4 })
        |> GameMap.spawnEntity (Point.create 3 2) EntityId.create (BoxEntity ())
        |> GameMap.spawnEntity (Point.create 4 2) EntityId.create (BalloonEntity { color = Blue })
        |> GameMap.updateTile (Point.create 6 2) (InkTile { color = Red })
        |> GameMap.updateTile (Point.create 7 2) (InkTile { color = Green })
        |> GameMap.updateTile (Point.create 8 2) (PinTile { color = Red })
        |> GameMap.updateTile (Point.create 8 3) (PinTile { color = Green })
        |> GameMap.updateTile (Point.create 10 1) (TeleporterTile { targetPosition = Point.create 10 6; isActive = true })
        |> GameMap.updateTile (Point.create 10 6) (TeleporterTile { targetPosition = Point.create 10 2; isActive = true })
        |> GameMap.updateTile (Point.create 2 2) (CornerTile { orientation = West })
        |> GameMap.updateTile (Point.create 2 4) (CornerTile { orientation = East })
        |> GameMap.updateTile (Point.create 2 3) (TeleporterTile { targetPosition = Point.create 2 2; isActive = true })
        |> GameMap.updateTile (Point.create 2 1) (TeleporterTile { targetPosition = Point.create 2 2; isActive = true })
        |> GameMap.updateTile (Point.create 12 3) (PistonTile { orientation = North; triggerPosition = Point.create 4 4; force = 2; isExtended = false })
        |> GameMap.spawnEntity (Point.create 12 3) EntityId.create (PistonEntity { orientation = North })
        |> GameMap.updateTile (Point.create 12 4) (CornerTile { orientation = South })
        //|> GameMap.updateTile (Point.create 12 4) (TeleporterTile { targetPosition = Point.create 2 6; isActive = true })
        |> GameMap.spawnEntity (Point.create 12 4) EntityId.create (BoxEntity ())
        |> GameMap.spawnEntity (Point.create 1 1) EntityId.create (PlayerEntity { name = "Player"; orientation = East })