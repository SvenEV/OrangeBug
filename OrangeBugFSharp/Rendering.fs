namespace OrangeBug

module Rendering =
    open System
    open Behaviors
    open TilesEntities
    open GameMap

    let tileAsString tile =
        match tile with
        | PathTile -> [| "   "; "   "; "   " |]
        | WallTile -> [| "███"; "███"; "███" |]
        | ButtonTile false -> [| "┌x┐"; "│ │"; "└─┘" |]
        | ButtonTile true -> [| "┌o┐"; "│ │"; "└─┘" |]
        | GateTile { isOpen = false } -> [| "▓▓▓"; "▓▓▓"; "▓▓▓" |]
        | GateTile { isOpen = true } -> [| "░░░"; "░░░"; "░░░" |]
        | _ -> [| "???"; "???"; "???" |]

    let entityAsString entity =
        match entity with
        | Some (PlayerEntity player) ->
            match player.orientation with
            | North -> "▲"
            | East -> "►"
            | South -> "▼"
            | West -> "◄"
        | Some BoxEntity -> "▥"
        | Some (BalloonEntity _) -> "O"
        | None -> " "

    let mapAsString (map: GameMap) =
        let rowAsString row =
            String.concat "\n" [
                Seq.map (fun (t: TileInfo) -> (tileAsString t.tile).[0]) row |> String.concat " "
                Seq.map (fun (t: TileInfo) -> [(tileAsString t.tile).[1].[0]; (entityAsString t.entity).[0]; (tileAsString t.tile).[1].[0]] |> Array.ofList |> String) row |> String.concat " "
                Seq.map (fun (t: TileInfo) -> (tileAsString t.tile).[2]) row |> String.concat " "
            ]

        let rows =
            map.toSeq
            |> Seq.chunkBySize map.size.x
            |> Seq.rev
            |> Seq.map rowAsString
            |> String.concat "\n\n"
        
        rows

