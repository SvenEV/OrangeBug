namespace OrangeBug

module Rendering =
    open System
    open OrangeBug.Game

    let tileAsString tile =
        match tile with
        | PathTile -> [| "   "; "   "; "   " |]
        | WallTile -> [| "███"; "███"; "███" |]
        | ButtonTile false -> [| "┌x┐"; "│ │"; "└─┘" |]
        | ButtonTile true -> [| "┌o┐"; "│ │"; "└─┘" |]
        | GateTile { isOpen = false } -> [| "▓▓▓"; "▓▓▓"; "▓▓▓" |]
        | GateTile { isOpen = true } -> [| "░░░"; "░░░"; "░░░" |]
        | _ -> [| "???"; "???"; "???" |]

    let entityAsString (map: GameMapState) entityId =
        match entityId with
        | Some id ->
            let _, entity = map |> GameMap.getEntity id
            match entity with
            | PlayerEntity player ->
                match player.orientation with
                | North -> "▲"
                | East -> "►"
                | South -> "▼"
                | West -> "◄"
            | BoxEntity -> "▥"
            | BalloonEntity _ -> "O"
        | None -> " "

    let mapAsString (map: GameMapState) =
        let rowAsString row =
            String.concat "\n" [
                Seq.map (fun (t: TileInfo) -> (tileAsString t.tile).[0]) row |> String.concat " "
                Seq.map (fun (t: TileInfo) -> [(tileAsString t.tile).[1].[0]; (entityAsString map t.entityId).[0]; (tileAsString t.tile).[1].[0]] |> Array.ofList |> String) row |> String.concat " "
                Seq.map (fun (t: TileInfo) -> (tileAsString t.tile).[2]) row |> String.concat " "
            ]

        let rows =
            let points = [0 .. map.size.y - 1] |> Seq.collect (fun y -> [0 .. map.size.x - 1] |> Seq.map (fun x -> Point.create x y))
            Seq.map (fun p -> GameMap.getAt p map) points
            |> Seq.chunkBySize map.size.x
            |> Seq.rev
            |> Seq.map rowAsString
            |> String.concat "\n\n"
        
        rows

