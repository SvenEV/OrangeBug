import { Object3D, Euler, Vector3, MaxEquation } from "three";
import { Entity, Point, Direction } from "./CommonTypes";
import { MeshFactory } from "./MeshFactory";

class MoveAnimation {
    startPosition: Vector3
    endPosition: Vector3
    startTime: number
    duration: number
}

export class EntityVisual extends Object3D {
    
    private _entity: Entity;
    private _orientation: Direction;

    moveAnimation: MoveAnimation;
    
    get entity() { return this._entity; }
    
    set entity(value) {
        this._entity = value;
        this.updateVisual();
    }
    
    updateVisual() {
        let entity = this._entity;
        this.remove(this.children[0]);
        this.add(MeshFactory.getMesh(entity));
        if (entity.$type === "PlayerEntity") {
            switch (entity.state.orientation) {
                case Direction.North:
                    this.setRotationFromEuler(new Euler(0, 0, 0));
                    break;
                case Direction.East:
                    this.setRotationFromEuler(new Euler(0, 0, 1.5 * Math.PI));
                    break;
                case Direction.South:
                    this.setRotationFromEuler(new Euler(0, 0, Math.PI));
                    break;
                case Direction.West:
                    this.setRotationFromEuler(new Euler(0, 0, .5 * Math.PI));
                    break;
            }
        }
        else if (entity.$type === "PistonEntity") {
            switch (entity.state.orientation) {
                case Direction.North:
                    this.setRotationFromEuler(new Euler(0, 0, 0));
                    break;
                case Direction.East:
                    this.setRotationFromEuler(new Euler(0, 0, 1.5 * Math.PI));
                    break;
                case Direction.South:
                    this.setRotationFromEuler(new Euler(0, 0, Math.PI));
                    break;
                case Direction.West:
                    this.setRotationFromEuler(new Euler(0, 0, .5 * Math.PI));
                    break;
            }
        }
        else {
            this.setRotationFromEuler(new Euler(0, 0, 0));
        }
    }

    get orientation() {
        return this._orientation;
    }
    
    set orientation(o) {
        this._orientation = o;
    }

    constructor(entity: Entity, position: Point) {
        super();
        this.entity = entity;
        this.position.set(position.x, position.y, 1);
    }

    update(time: number, deltaTime: number) {
        if (this.moveAnimation) {
            let progress = Math.min(1, Math.max(0, time - this.moveAnimation.startTime) / this.moveAnimation.duration)
            progress = Math.sin(progress * (Math.PI/2))
            this.position.lerpVectors(this.moveAnimation.startPosition, this.moveAnimation.endPosition, progress)
        }

        let targetRotation: number;
        switch (this._orientation) {
            case Direction.North:
                targetRotation = 0;
                break;
            case Direction.East:
                targetRotation = 1.5 * Math.PI;
                break;
            case Direction.South:
                targetRotation = Math.PI;
                break;
            case Direction.West:
                targetRotation = .5 * Math.PI;
                break;
        }
        let rotation = EntityVisual.lerpAngle(this.rotation.z, targetRotation, 10 * deltaTime);
        this.setRotationFromEuler(new Euler(0, 0, rotation));
    }
    
    private static lerpAngle(a: number, b: number, t: number) {
        let lerp = function (a: number, b: number, t: number) {
            return (1 - t) * a + t * b;
        };
        let result;
        let diff = b - a;
        if (diff < -Math.PI) {
            // lerp upwards past 2 * Math.PI
            b += 2 * Math.PI;
            result = lerp(a, b, t);
            if (result >= 2 * Math.PI)
                result -= 2 * Math.PI;
        }
        else if (diff > Math.PI) {
            // lerp downwards past 0
            b -= 2 * Math.PI;
            result = lerp(a, b, t);
            if (result < 0)
                result += 2 * Math.PI;
        }
        else {
            // straight lerp
            result = lerp(a, b, t);
        }
        return result;
    }
}