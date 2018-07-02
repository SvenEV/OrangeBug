namespace OrangeBug

module Effects =
    open TilesEntities
    open IntentsEvents
    open GameMapTypes

    // Effects
    
    type TileUpdateEffect = {
        position: Point
        tile: Tile
    }

    type EntityUpdateEffect = {
        entityId: EntityId
        entity: Entity
    }

    type EntitySpawnEffect = {
        position: Point
        entity: Entity
        entityId: EntityId
    }

    type EntityDespawnEffect = {
        entityId: EntityId
    }

    type EntityMoveEffect = {
        entityId: EntityId
        newPosition: Point
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
    

    // "Intents" are operations to do - they may or may not be successful.
    //           Handling an intent may generate (1) events that are returned
    //           and (2) further intents that are handled recursively.
    //
    // "Events" are high-level changes to the game world. They are often specific
    //          to certain tile or entity types. Events are emitted when intents
    //          (fully or partially) succeed. Each event translates to one or
    //          more effects.
    //
    // "Effects" are low-level changes to the game world. Think of them as instructions
    //           that can be directly handled by the core game engine. Examples
    //           include updating a tile or entity state, or playing a sound effect.

    let eventToEffects (map: MapAccessor) ev =
        match ev with
        | EntityMovedEvent ev ->
            EntityMoveEffect { entityId = ev.entityId; newPosition = ev.newPosition } :: []

        | PlayerRotatedEvent ev ->
            let playerId = map.getPlayerId ev.name
            let _, (PlayerEntity playerState) = map.getEntity playerId
            let newState = { playerState with orientation = ev.orientation }
            [
                SoundEffect { key = "RotatePlayer.mp3" }
                EntityUpdateEffect { entityId = playerId; entity = PlayerEntity newState }
            ]
        
        | ButtonPressedEvent ev ->
            [ TileUpdateEffect { position = ev.position; tile = ButtonTile true }]

        | ButtonReleasedEvent ev ->
            [ TileUpdateEffect { position = ev.position; tile = ButtonTile false }]

        | GateOpenedEvent ev ->
            let (GateTile gate) = (map.getAt ev.position).tile
            [ TileUpdateEffect { position = ev.position; tile = GateTile { gate with isOpen = true } } ]

        | GateClosedEvent ev ->
            let (GateTile gate) = (map.getAt ev.position).tile
            [ TileUpdateEffect { position = ev.position; tile = GateTile { gate with isOpen = false } } ]

        | BalloonColoredEvent ev ->
            [
                SoundEffect { key = "ColorBalloon.mp3" }
                EntityUpdateEffect { entityId = ev.entityId; entity = BalloonEntity ev.color }
                TileUpdateEffect { position = ev.inkPosition; tile = PathTile }
            ]
        | BalloonPoppedEvent entityId ->
            [
                SoundEffect { key = "PopBalloon.mp3" }
                EntityDespawnEffect { entityId = entityId }
            ]

