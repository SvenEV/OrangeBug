import { Euler } from "three";

export type Tile = { $type: string, state: { [key: string]: any } }
export type Entity = { $type: string, state: { [key: string]: any } }
export type Effect = { $type: string, props: { [key: string]: any } }
export type GameEvent = { $type: string, props: { [key: string]: any } }

export class Point {
    x: number
    y: number

    constructor(x: number, y: number) {
        this.x = x
        this.y = y
    }
}

export enum Direction { North = "North", East = "East", South = "South", West = "West" }

export type EntityId = { id: number }

export type EntityEntry = { entity: Entity, position: Point }

export type GameMap = {
    size: Point,
    tiles: [Tile],
    entities: [{ key: EntityId, value: EntityEntry }]
}

export function toEuler(direction: Direction) {
    switch (direction) {
        case "North": return new Euler(0, 0, 0);
        case "East": return new Euler(0, 0, 1.5 * Math.PI);
        case "South": return new Euler(0, 0, Math.PI);
        case "West": return new Euler(0, 0, .5 * Math.PI);
    }
}