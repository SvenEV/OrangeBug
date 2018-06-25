namespace OrangeBug

module GameMap =
    open TilesEntities
    open Behaviors
    open Effects
    open Newtonsoft.Json

    type GameMap =
        {
            size: Point
            tiles: Tile[]
            
            [<field: JsonIgnore>]
            mutable entities: Map<Point, Entity>

            [<field: JsonIgnore>]
            mutable players: Map<string, Point>
        }

        member private map.pointToIndex (p: Point) =
            p.y * map.size.x + p.x

        static member create width height = {
            size = Point.create width height

            players = Map.ofList [ "Player", Point.create 1 1 ]
        
            entities = Map.ofList [ Point.create 1 1, PlayerEntity { name = "Player"; orientation = East } ]
            
            tiles = Array.init (width * height) (fun i ->
                let x, y = (i % width, i / width)
                match (x, y) with
                | (0, _) -> WallTile
                | (_, 0) -> WallTile
                | (x, _) when x = width - 1 -> WallTile
                | (_, y) when y = height - 1 -> WallTile
                | _ -> PathTile)
        }

        member map.getAt p =
            let i = map.pointToIndex p
            {
                position = p
                tile = map.tiles.[i]
                entity = Map.tryFind p map.entities
            }

        member map.getPlayerPosition name =
            Map.find name map.players

        member map.getPlayer name =
            let playerPos = Map.find name map.players
            let playerTile = map.getAt playerPos
            let player =
                match playerTile.entity with
                | Some (PlayerEntity e) -> Some e
                | _ -> None

            playerPos, player.Value

        member map.setAt p tile entity =
            let i = map.pointToIndex p
            map.tiles.[i] <- tile
            match entity with
            | Some theEntity -> map.entities <- Map.add p theEntity map.entities
            | None -> map.entities <- Map.remove p map.entities
        
        [<JsonIgnore>]
        member map.toSeq =
            let points = [0 .. map.size.y - 1] |> Seq.collect (fun y -> [0 .. map.size.x - 1] |> Seq.map (fun x -> Point.create x y))
            Seq.map map.getAt points

        member private map.applyEffect effect =
            match effect with
            | TileUpdateEffect e ->
                let current = map.getAt e.position
                map.setAt e.position e.tile current.entity

            | EntityUpdateEffect e ->
                let current = map.getAt e.position
                map.setAt e.position current.tile (Some e.entity)

            | EntityMoveEffect e ->
                let currentSource = map.getAt e.sourcePosition
                map.setAt e.sourcePosition currentSource.tile None
                let currentTarget = map.getAt e.targetPosition
                map.setAt e.targetPosition currentTarget.tile currentSource.entity

                match currentTarget.entity with
                | Some (PlayerEntity player) -> map.players <- Map.remove player.name map.players
                | _ -> ()

                match currentSource.entity with
                | Some (PlayerEntity player) -> map.players <- Map.add player.name e.targetPosition map.players
                | _ -> ()

            | EntitySpawnEffect e ->
                let current = map.getAt e.position
                map.setAt e.position current.tile (Some e.entity)

                match current.entity with
                | Some (PlayerEntity player) -> map.players <- Map.remove player.name map.players
                | _ -> ()

                match e.entity with
                | PlayerEntity player -> map.players <- Map.add player.name e.position map.players
                | _ -> ()
     
            | EntityDespawnEffect e ->
                let current = map.getAt e.position
                map.setAt e.position current.tile None

                match current.entity with
                | Some (PlayerEntity player) -> map.players <- Map.remove player.name map.players
                | _ -> ()

            | SoundEffect e -> ()

        member map.applyEffects effects =
            List.iter (fun e -> map.applyEffect e) effects

        [<JsonIgnore>]
        member map.accessor = {
            getAt = map.getAt
            getPlayerPosition = map.getPlayerPosition
            getPlayer = map.getPlayer
            handleIntent = Gameplay.handleIntent
        }