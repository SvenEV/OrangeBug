namespace OrangeBug.Game

open OrangeBug

type PlayerRotatedEvent = { name: string; entityId: EntityId; player: PlayerEntity; orientation: Direction; }
type EntityAttachedEvent = { entityId: EntityId; position: Point }
type EntityDetachedEvent = { entityId: EntityId; position: Point }
type EntityMovedEvent = { entityId: EntityId; oldPosition: Point; newPosition: Point }
type ButtonPressedEvent = { position: Point }
type ButtonReleasedEvent = { position: Point }
type GateOpenedEvent = { position: Point; gate: GateTile }
type GateClosedEvent = { position: Point; gate: GateTile }
type BalloonColoredEvent = { entityId: EntityId; inkPosition: Point; color: InkColor }
type BalloonPoppedEvent = { entityId: EntityId; pinPosition: Point }
type TeleporterDeactivatedEvent = { position: Point; teleporter: TeleporterTile }
type TeleporterActivatedEvent = { position: Point; teleporter: TeleporterTile }
type PistonExtendedEvent = { position: Point; piston: PistonTile }
type PistonRetractedEvent = { position: Point; piston: PistonTile }

type Event =
    | PlayerRotatedEvent of PlayerRotatedEvent
    | EntityAttachedEvent of EntityAttachedEvent
    | EntityDetachedEvent of EntityDetachedEvent
    | EntityMovedEvent of EntityMovedEvent
    | ButtonPressedEvent of ButtonPressedEvent
    | ButtonReleasedEvent of ButtonReleasedEvent
    | GateOpenedEvent of GateOpenedEvent
    | GateClosedEvent of GateClosedEvent
    | BalloonColoredEvent of BalloonColoredEvent
    | BalloonPoppedEvent of BalloonPoppedEvent
    | TeleporterDeactivatedEvent of TeleporterDeactivatedEvent
    | TeleporterActivatedEvent of TeleporterActivatedEvent
    | PistonExtendedEvent of PistonExtendedEvent
    | PistonRetractedEvent of PistonRetractedEvent