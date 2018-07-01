namespace OrangeBug

module TilesEntities =

    let mutable nextEntityId = 0

    type EntityId = 
        { id: int }
        static member create =
            nextEntityId <- nextEntityId + 1
            { id = nextEntityId }
    

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