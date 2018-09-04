namespace OrangeBug

open System
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing;
open OrangeBug.Game

module Rendering =
    [<RequireQualifiedAccess>]
    module ImageCache =
        let spriteSize = 16
        let mutable images = Map.empty

        let load name =
            let image = Image.Load<Rgba32>(@"Assets\Sprites\" + name + ".png")
            image.Mutate(fun ctx -> ctx.Resize(SixLabors.Primitives.Size(spriteSize, spriteSize)) |> ignore)
            image
        
        let get name =
            match images.TryFind name with
            | Some image -> image
            | None ->
                let image = load name
                images <- images |> Map.add name image
                image

    let tileSpriteKey =
        function
        | PathTile -> "Path"
        | WallTile -> "Wall"
        | LockedTile -> "" // shouldn't happen
        | InkTile state -> "Ink" + (match state.color with Red -> "Red" | Green -> "Green" | Blue -> "Blue")
        | PinTile state -> "Pin" + (match state.color with Red -> "Red" | Green -> "Green" | Blue -> "Blue")
        | ButtonTile _ -> "Button"
        | GateTile state -> if state.isOpen then "GateOpened" else "GateClosed"
        | TeleporterTile _ -> "Teleporter"
        | CornerTile _ -> "Corner"
        | PistonTile _ -> "Piston"

    let entitySpriteKey =
        function
        | PlayerEntity _ -> "Player"
        | BoxEntity _ -> "Box"
        | BalloonEntity state -> "Balloon" + (match state.color with Red -> "Red" | Green -> "Green" | Blue -> "Blue")
        | PistonEntity _ -> "PistonEntity"

    let mapAsImage (map: GameMapState) =
        let tileSize = ImageCache.spriteSize
        let image = new Image<Rgba32>(map.size.x * tileSize, map.size.y * tileSize)
        
        for y in [0 .. map.size.y - 1] do
            for x in [0 .. map.size.x - 1] do
                let p = Point.create x y
                let drawTile (ctx: IImageProcessingContext<Rgba32>) =
                    let sprite = (map.tiles.Find p).tile |> tileSpriteKey |> ImageCache.get
                    let location = SixLabors.Primitives.Point(tileSize * x, image.Height - tileSize * y)
                    ctx.DrawImage(GraphicsOptions.Default, sprite, location) |> ignore
                image.Mutate(drawTile)

        for e in map.entities do
            let drawEntity (ctx: IImageProcessingContext<Rgba32>) =
                let p = e.Value.position
                let sprite = e.Value.entity |> entitySpriteKey |> ImageCache.get
                let location = SixLabors.Primitives.Point(tileSize * p.x, image.Height - tileSize * p.y)
                ctx.DrawImage(GraphicsOptions.Default, sprite, location) |> ignore
            image.Mutate(drawEntity)

        image


module TextRendering =
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