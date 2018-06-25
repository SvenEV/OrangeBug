namespace OrangeBug

open GameMap
open IntentsEvents
open Effects

module Game =
    open TilesEntities

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

    let eventToEffects (map: GameMap) ev =
        match ev with
        | EntityMovedEvent ev ->
            EntityMoveEffect { sourcePosition = ev.sourcePosition; targetPosition = ev.targetPosition } :: []
        | PlayerRotatedEvent ev ->
            let (playerPos, playerState) = map.getPlayer ev.name
            let newState = { playerState with orientation = ev.orientation }
            [
                SoundEffect { key = "RotatePlayer.mp3" }
                EntityUpdateEffect { position = playerPos; entity = PlayerEntity newState }
            ]
        | BalloonColoredEvent ev ->
            [
                SoundEffect { key = "ColorBalloon.mp3" }
                EntityUpdateEffect { position = ev.position; entity = BalloonEntity ev.color }
            ]
        | BalloonPoppedEvent position ->
            [
                SoundEffect { key = "PopBalloon.mp3" }
                EntityDespawnEffect { position = position }
            ]

    