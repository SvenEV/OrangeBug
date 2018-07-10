﻿namespace OrangeBug.Game

open OrangeBug

// "Intents" are operations to do - they may or may not be successful.
//           Handling an intent may generate (1) events that are returned
//           and (2) further intents that are handled recursively.
//
// "Events" are high-level changes to the game world. They are often specific
//          to certain tile or entity types. Events are emitted when intents
//          (fully or partially) succeed. Each event translates to one or more
//          effects purely (i.e. without querying on the map state).
//
// "Effects" are low-level changes to the game world. Think of them as instructions
//           that can be directly handled by the core game engine. Examples
//           include updating a tile or entity state, or playing a sound effect.

// Effects
    
type TileUpdateEffect = {
    position: Point
    tile: Tile
}

type DependenciesUpdateEffect = {
    position: Point
    newDependencies: MapDependency list
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
    position: Point
}

type EntityMoveEffect = {
    entityId: EntityId
    oldPosition: Point
    newPosition: Point
}

type SoundEffect = {
    key: string
}

type Effect =
    | TileUpdateEffect of props: TileUpdateEffect
    | DependenciesUpdateEffect of props: DependenciesUpdateEffect
    | EntityUpdateEffect of props: EntityUpdateEffect
    | EntitySpawnEffect of props: EntitySpawnEffect
    | EntityDespawnEffect of props: EntityDespawnEffect
    | EntityMoveEffect of props: EntityMoveEffect
    | SoundEffect of props: SoundEffect

module Effect =

    let eventToEffects ev =
        match ev with
        | EntityAttachedEvent ev -> []
        | EntityDetachedEvent ev -> []
        | EntityMovedEvent ev ->
            [ EntityMoveEffect { entityId = ev.entityId; oldPosition = ev.oldPosition; newPosition = ev.newPosition } ]

        | DependenciesUpdatedEvent ev ->
            [ DependenciesUpdateEffect { position = ev.position; newDependencies = ev.newDependencies }]

        | PlayerRotatedEvent ev ->
            let newState = { ev.player with orientation = ev.orientation }
            [
                SoundEffect { key = "RotatePlayer.mp3" }
                EntityUpdateEffect { entityId = ev.entityId; entity = PlayerEntity newState }
            ]

        | ButtonPressedEvent ev ->
            [ TileUpdateEffect { position = ev.position; tile = ButtonTile true }]

        | ButtonReleasedEvent ev ->
            [ TileUpdateEffect { position = ev.position; tile = ButtonTile false }]

        | GateOpenedEvent ev ->
            [ TileUpdateEffect { position = ev.position; tile = GateTile { ev.gate with isOpen = true } } ]

        | GateClosedEvent ev ->
            [ TileUpdateEffect { position = ev.position; tile = GateTile { ev.gate with isOpen = false } } ]

        | BalloonColoredEvent ev ->
            [
                SoundEffect { key = "ColorBalloon.mp3" }
                EntityUpdateEffect { entityId = ev.entityId; entity = BalloonEntity ev.color }
                TileUpdateEffect { position = ev.inkPosition; tile = PathTile }
            ]

        | BalloonPoppedEvent ev ->
            [
                SoundEffect { key = "PopBalloon.mp3" }
                EntityDespawnEffect { entityId = ev.entityId; position = ev.pinPosition }
            ]

        | TeleporterDeactivatedEvent ev ->
            [ TileUpdateEffect { position = ev.position; tile = TeleporterTile { ev.teleporter with isActive = false } }]

        | TeleporterActivatedEvent ev ->
            [ TileUpdateEffect { position = ev.position; tile = TeleporterTile { ev.teleporter with isActive = true } }]

        | PistonExtendedEvent ev ->
            [ TileUpdateEffect { position = ev.position; tile = PistonTile { ev.piston with isExtended = true } } ]

        | PistonRetractedEvent ev ->
            [ TileUpdateEffect { position = ev.position; tile = PistonTile { ev.piston with isExtended = false } } ]

