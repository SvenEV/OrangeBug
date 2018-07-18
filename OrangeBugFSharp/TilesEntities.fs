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

type InkTile = {
    color: InkColor
}

type PinTile = {
    color: InkColor
}

type ButtonTile = {
    isPressed: bool
}

type GateTile = {
    triggerPosition: Point
    isOpen: bool
}

type TeleporterTile = {
    targetPosition: Point
    isActive: bool
}

type CornerTile = {
    orientation: Direction
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
    | LockedTile
    | InkTile of state: InkTile
    | PinTile of state: PinTile
    | ButtonTile of state: ButtonTile
    | GateTile of state: GateTile
    | TeleporterTile of state: TeleporterTile
    | CornerTile of state: CornerTile
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

type BalloonEntity = {
    color: InkColor
}

type PistonEntity = {
    orientation: Direction
}

type Entity =
    | PlayerEntity of state: PlayerEntity
    | BoxEntity
    | BalloonEntity of state: BalloonEntity
    | PistonEntity of state: PistonEntity

    
type TileInfo = {
    tile: Tile
    entityId: EntityId option
    position: Point
}