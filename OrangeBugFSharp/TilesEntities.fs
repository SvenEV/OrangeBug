namespace OrangeBug.Game

open OrangeBug

module EntityId =
    let mutable nextEntityId = 0

type EntityId = 
    { id: int }
    static member create =
        EntityId.nextEntityId <- EntityId.nextEntityId + 1
        { id = EntityId.nextEntityId }

// Tiles
    
type GateTile = {
    triggerPosition: Point
    isOpen: bool
}

type TeleporterTile = {
    targetPosition: Point
    isActive: bool
}

type PistonTile = {
    orientation: Direction
    triggerPosition: Point
    force: int
    isExtended: bool
}

type Tile =
    | PathTile
    | WallTile
    | InkTile of color: InkColor
    | PinTile of color: InkColor
    | ButtonTile of isPressed: bool
    | GateTile of state: GateTile
    | TeleporterTile of state: TeleporterTile
    | CornerTile of orientation: Direction
    | PistonTile of state: PistonTile

module CornerTile =
    let mapInToOutDirection orientation inDirection =
        match orientation, inDirection with
        | North, Some South -> Some East
        | North, Some West -> Some North
        | East, Some West -> Some South
        | East, Some North -> Some East
        | South, Some North -> Some West
        | South, Some East -> Some South
        | West, Some East -> Some North
        | West, Some South -> Some West
        | _ -> None
    let isValidOutDirection orientation outDirection =
        match outDirection with
        | None -> false
        | Some outDirection ->
            List.contains
                outDirection
                (match orientation with
                | North -> [ North; East ]
                | East -> [ East; South ]
                | South -> [ South; West ]
                | West -> [ West; North ])

// Entities
    
type PlayerEntity = {
    name: string
    orientation: Direction
}

type Entity =
    | PlayerEntity of state: PlayerEntity
    | BoxEntity
    | BalloonEntity of color: InkColor
    | PistonEntity of orientation: Direction

    
type TileInfo = {
    tile: Tile
    entityId: EntityId option
    position: Point
}