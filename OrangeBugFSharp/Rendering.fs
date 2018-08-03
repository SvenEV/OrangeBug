namespace OrangeBug

module Rendering =
    open System
    open OrangeBug.Game

    let tileAsString tile =
        match tile with
        | PathTile -> [| "     "; "     "; "     " |]
        | WallTile -> [| "█████"; "█████"; "█████" |]
        | ButtonTile { isPressed = false } -> [| "┌-x-┐"; "│   │"; "└───┘" |]
        | ButtonTile { isPressed = true } -> [| "┌-o-┐"; "│   │"; "└───┘" |]
        | GateTile { isOpen = false } -> [| "▓▓▓▓▓"; "▓▓▓▓▓"; "▓▓▓▓▓" |]
        | GateTile { isOpen = true } -> [| "░░░░░"; "░░░░░"; "░░░░░" |]
        | TeleporterTile _ -> [| " *** "; "*   *"; " *** " |]
        | CornerTile { orientation = North } -> [| "||   "; "||   "; "\\====" |]
        | CornerTile { orientation = East } -> [| "/===="; "||   "; "||   " |]
        | CornerTile { orientation = South } -> [| "====\\"; "   ||"; "   ||" |]
        | CornerTile { orientation = West } -> [| "   ||"; "   ||"; "====/" |]
        | _ -> [| " ??? "; "?????"; " ??? " |]

    let entityAsString (map: GameMapState) entityId =
        let entity = entityId |> Option.bind (fun id -> GameMap.tryGetEntity id map) |> Option.map snd
        match entity with
        | Some (PlayerEntity player) ->
            match player.orientation with
            | North -> "▲"
            | East -> "►"
            | South -> "▼"
            | West -> "◄"
        | Some (BoxEntity _) -> "▥"
        | Some (BalloonEntity _) -> "O"
        | Some (PistonEntity _) -> "&"
        | None -> " "

    let mapAsString (map: GameMapState) =
        let rowAsString row =
            String.concat "\n" [
                Seq.map (fun (tile, _) -> (tileAsString tile).[0]) row |> String.concat " "
                Seq.map (fun (tile, entityId) -> [(tileAsString tile).[1].[0]; (tileAsString tile).[1].[1]; (entityAsString map entityId).[0]; (tileAsString tile).[1].[3]; (tileAsString tile).[1].[4] ] |> Array.ofList |> String) row |> String.concat " "
                Seq.map (fun (tile, _) -> (tileAsString tile).[2]) row |> String.concat " "
            ]

        let rows =
            let points = [0 .. map.size.y - 1] |> Seq.collect (fun y -> [0 .. map.size.x - 1] |> Seq.map (fun x -> Point.create x y))
            Seq.map (fun p -> GameMap.getTile p map) points
            |> Seq.chunkBySize map.size.x
            |> Seq.rev
            |> Seq.map rowAsString
            |> String.concat "\n\n"
        
        rows

