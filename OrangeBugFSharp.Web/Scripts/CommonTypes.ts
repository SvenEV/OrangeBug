type UnionCase = { $type: string, [key: string]: any }

export type Tile = UnionCase
export type Entity = UnionCase
export type Effect = UnionCase

export class Point {
    x: number
    y: number

    constructor(x: number, y: number) {
        this.x = x
        this.y = y
    }
}

export enum Direction { North = "North", East = "East", South = "South", West = "West" }

export type GameMap = {
    size: Point,
    tiles: [Tile],
    entities: [{ key: Point, value: Entity }]
}
