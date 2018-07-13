namespace OrangeBug.Game

open OrangeBug

type PlayerRotatedEvent = { name: string; entityId: EntityId; player: PlayerEntity; orientation: Direction; }
type EntityMovedEvent = { entityId: EntityId; oldPosition: Point; newPosition: Point }
type ButtonPressedEvent = { position: Point; button: ButtonTile }
type ButtonReleasedEvent = { position: Point; button: ButtonTile }
type GateOpenedEvent = { position: Point; gate: GateTile }
type GateClosedEvent = { position: Point; gate: GateTile }
type BalloonColoredEvent = { entityId: EntityId; inkPosition: Point; color: InkColor; balloon: BalloonEntity }
type BalloonPoppedEvent = { entityId: EntityId; pinPosition: Point }
type TeleporterDeactivatedEvent = { position: Point; teleporter: TeleporterTile }
type TeleporterActivatedEvent = { position: Point; teleporter: TeleporterTile }
type PistonExtendedEvent = { position: Point; piston: PistonTile }
type PistonRetractedEvent = { position: Point; piston: PistonTile }

type Event =
    | PlayerRotatedEvent of props: PlayerRotatedEvent
    | EntityMovedEvent of props: EntityMovedEvent
    | ButtonPressedEvent of props: ButtonPressedEvent
    | ButtonReleasedEvent of props: ButtonReleasedEvent
    | GateOpenedEvent of props: GateOpenedEvent
    | GateClosedEvent of props: GateClosedEvent
    | BalloonColoredEvent of props: BalloonColoredEvent
    | BalloonPoppedEvent of props: BalloonPoppedEvent
    | TeleporterDeactivatedEvent of props: TeleporterDeactivatedEvent
    | TeleporterActivatedEvent of props: TeleporterActivatedEvent
    | PistonExtendedEvent of props: PistonExtendedEvent
    | PistonRetractedEvent of props: PistonRetractedEvent