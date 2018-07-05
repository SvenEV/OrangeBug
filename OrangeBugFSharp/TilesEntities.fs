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

type Tile =
    | PathTile
    | WallTile
    | InkTile of color: InkColor
    | PinTile of color: InkColor
    | ButtonTile of isPressed: bool
    | GateTile of state: GateTile
    | TeleporterTile of targetPosition: Point
    

// Entities
    
type PlayerEntity = {
    name: string
    orientation: Direction
}

type Entity =
    | PlayerEntity of state: PlayerEntity
    | BoxEntity
    | BalloonEntity of color: InkColor

    
type TileInfo = {
    tile: Tile
    entityId: EntityId option
    position: Point
}