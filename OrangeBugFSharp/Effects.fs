namespace OrangeBug

module Effects =
    open TilesEntities

    // Effects
    type TileUpdateEffect = {
        position: Point
        tile: Tile
    }

    type EntityUpdateEffect = {
        position: Point
        entity: Entity
    }

    type EntitySpawnEffect = {
        position: Point
        entity: Entity
    }

    type EntityDespawnEffect = {
        position: Point
    }

    type EntityMoveEffect = {
        sourcePosition: Point
        targetPosition: Point
    }

    type SoundEffect = {
        key: string
    }

    type Effect =
        | TileUpdateEffect of props: TileUpdateEffect
        | EntityUpdateEffect of props: EntityUpdateEffect
        | EntitySpawnEffect of props: EntitySpawnEffect
        | EntityDespawnEffect of props: EntityDespawnEffect
        | EntityMoveEffect of props: EntityMoveEffect
        | SoundEffect of props: SoundEffect

