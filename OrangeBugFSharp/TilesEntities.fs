namespace OrangeBug

module TilesEntities =

    // Tile states
    type GateTile = {
        triggerPosition: Point
        isOpen: bool
    }

    type Tile =
        | PathTile
        | WallTile
        | InkTile of color: InkColor
        | PinTile of color: InkColor
        | ButtonTile of isPressed: bool // do we need an explicit IsOn-bool?
        | GateTile of state: GateTile // do we need an explicit IsOpen-bool?
    
    // Entity states
    type PlayerEntity = {
        name: string
        orientation: Direction
    }

    type Entity =
        | PlayerEntity of state: PlayerEntity
        | BoxEntity
        | BalloonEntity of color: InkColor