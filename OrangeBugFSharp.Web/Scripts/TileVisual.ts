import { Object3D, Euler } from "three";
import { Tile, Point } from "./CommonTypes";
import { MeshFactory } from "./MeshFactory";

export class TileVisual extends Object3D {
    private _tile: Tile;
    get tile() { return this._tile; }
    set tile(value) {
        this._tile = value;
        this.updateVisual();
    }
    updateVisual() {
        let tile = this._tile;
        this.remove(this.children[0]);
        this.add(MeshFactory.getMesh(tile));
        if (tile.$type === "CornerTile") {
            switch (tile.state.orientation) {
                case "West":
                    this.setRotationFromEuler(new Euler(0, 0, 0));
                    break;
                case "North":
                    this.setRotationFromEuler(new Euler(0, 0, 1.5 * Math.PI));
                    break;
                case "East":
                    this.setRotationFromEuler(new Euler(0, 0, Math.PI));
                    break;
                case "South":
                    this.setRotationFromEuler(new Euler(0, 0, .5 * Math.PI));
                    break;
            }
        }
        else if (tile.$type === "PistonTile") {
            switch (tile.state.orientation) {
                case "North":
                    this.setRotationFromEuler(new Euler(0, 0, 0));
                    break;
                case "East":
                    this.setRotationFromEuler(new Euler(0, 0, 1.5 * Math.PI));
                    break;
                case "South":
                    this.setRotationFromEuler(new Euler(0, 0, Math.PI));
                    break;
                case "West":
                    this.setRotationFromEuler(new Euler(0, 0, .5 * Math.PI));
                    break;
            }
        }
        else {
            this.setRotationFromEuler(new Euler(0, 0, 0));
        }
    }
    get mapPosition() {
        return { x: this.position.x, y: this.position.y };
    }
    set mapPosition(position) {
        this.position.set(position.x, position.y, 0);
    }
    constructor(tile: Tile, position: Point) {
        super();
        this.tile = tile;
        this.mapPosition = position;
    }
}