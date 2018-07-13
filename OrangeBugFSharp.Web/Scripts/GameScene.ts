﻿import * as Three from "three"
import { Camera, Scene, PerspectiveCamera, WebGLRenderer, BoxGeometry, MeshBasicMaterial, Mesh, TextureLoader, Texture, Audio, AudioLoader, PlaneGeometry, MeshStandardMaterial, DirectionalLight, WebGLShadowMap, ShadowMapType, PCFSoftShadowMap, DirectionalLightHelper, AmbientLight, Object3D, Geometry, Material, AudioListener, AudioBuffer, CylinderGeometry, OrthographicCamera, Euler, AnimationClip, Vector2, Clock, Vector3, Quaternion } from "three"
import { Tile, Entity, Effect, Point, Direction, GameMap, EntityId, GameEvent } from "./CommonTypes"

class GameAssets {

    private static readonly textureLoader = new TextureLoader()
    private static readonly audioLoader = new AudioLoader()
    private static readonly loadedSprites: { [key: string]: Texture } = {}
    private static readonly loadedSounds: { [key: string]: Audio } = {}
    private static audioListener: AudioListener

    static initialize(audioListener: AudioListener) {
        this.audioListener = audioListener
    }

    static getSprite(key: string) {
        let sprite = this.loadedSprites[key]

        if (sprite)
            return sprite

        let texture = this.textureLoader.load("images/sprites/" + key + ".png")
        this.loadedSprites[key] = texture
        return texture
    }

    static getSoundAsync(key: string): Promise<Audio> {
        return new Promise((resolve, reject) => {
            let sound = this.loadedSounds[key]

            if (sound)
                resolve(sound)

            this.audioLoader.load("sounds/" + key + ".mp3", (buffer: AudioBuffer) => {
                let audio = new Audio(this.audioListener)
                audio.setBuffer(buffer)
                this.loadedSounds[key] = audio;
                resolve(audio)
            }, null, null)
        })
    }
}

class MeshFactory {

    private static readonly geometries: { [key: string]: Geometry } = {}
    private static readonly materials: { [key: string]: Material } = {}
    private static readonly meshGenerators: { [key: string]: (() => Mesh) } = {}

    static initialize() {
        this.geometries.Plane = new PlaneGeometry(1, 1)
        this.geometries.Cube = new BoxGeometry(1, 1, 1)

        this.materials.Default = new MeshStandardMaterial({ color: 0xff00ff });

        this.meshGenerators.Default = () => new Mesh(this.geometries.Plane, this.getMaterial("NoSprite"))
        this.meshGenerators.PathTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Path"))
        this.meshGenerators.WallTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Wall"))
        this.meshGenerators.ButtonTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Button"))
        this.meshGenerators.TeleporterTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Teleport"))
        this.meshGenerators.CornerTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Corner"))
        this.meshGenerators.PistonTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Piston"))
        this.meshGenerators.PistonEntity = () => new Mesh(this.geometries.Plane, this.getMaterial("BoxingGlove"))
        this.meshGenerators.BoxEntity = () => new Mesh(this.geometries.Plane, this.getMaterial("Box"))
        this.meshGenerators.PlayerEntity = () => new Mesh(this.geometries.Plane, this.getMaterial("PlayerRight"))
    }

    private static getMaterial(spriteKey: string) {
        let material = this.materials[spriteKey]

        if (material)
            return material

        material = new MeshBasicMaterial({ map: GameAssets.getSprite(spriteKey), transparent: true });
        this.materials[spriteKey] = material
        return material
    }

    static getMesh(tileOrEntity: Tile | Entity): Mesh {
        switch (tileOrEntity.$type) {
            // special cases first (mesh depending on tile/entity state)
            case "GateTile": {
                return new Mesh(this.geometries.Plane, this.getMaterial(tileOrEntity.state.isOpen
                    ? "DummyWallRemoved"
                    : "DummyWall"))
            }

            case "InkTile": {
                return new Mesh(this.geometries.Plane, this.getMaterial(tileOrEntity.state.color + "Ink"))
            }

            case "PinTile": {
                return new Mesh(this.geometries.Plane, this.getMaterial(tileOrEntity.state.color + "Pool"))
            }

            case "BalloonEntity": {
                return new Mesh(this.geometries.Plane, this.getMaterial(tileOrEntity.state.color + "Ball"))
            }

            // simple cases (mesh only depending on tile/entity type)
            default: {
                let meshGenerator = this.meshGenerators[tileOrEntity.$type]
                if (!meshGenerator)
                    meshGenerator = this.meshGenerators.Default

                let mesh = meshGenerator()
                return mesh;
            }
        }
    }
}

class TileVisual extends Object3D {
    private _tile: Tile

    get tile() { return this._tile }

    set tile(value) {
        this._tile = value
        this.updateVisual()
    }

    updateVisual() {
        let tile = this._tile
        this.remove(this.children[0])
        this.add(MeshFactory.getMesh(tile))

        if (tile.$type === "CornerTile") {
            switch (tile.state.orientation) {
                case "West": this.setRotationFromEuler(new Euler(0, 0, 0)); break;
                case "North": this.setRotationFromEuler(new Euler(0, 0, 1.5 * Math.PI)); break;
                case "East": this.setRotationFromEuler(new Euler(0, 0, Math.PI)); break;
                case "South": this.setRotationFromEuler(new Euler(0, 0, .5 * Math.PI)); break;
            }
        } else if (tile.$type === "PistonTile") {
            switch (tile.state.orientation) {
                case "North": this.setRotationFromEuler(new Euler(0, 0, 0)); break;
                case "East": this.setRotationFromEuler(new Euler(0, 0, 1.5 * Math.PI)); break;
                case "South": this.setRotationFromEuler(new Euler(0, 0, Math.PI)); break;
                case "West": this.setRotationFromEuler(new Euler(0, 0, .5 * Math.PI)); break;
            }
        } else {
            this.setRotationFromEuler(new Euler(0, 0, 0))
        }
    }

    get mapPosition() {
        return { x: this.position.x, y: this.position.y }
    }

    set mapPosition(position) {
        this.position.set(position.x, position.y, 0)
    }

    constructor(tile: Tile, position: Point) {
        super()
        this.tile = tile
        this.mapPosition = position
    }
}

class EntityVisual extends Object3D {
    private _entity: Entity
    private _mapPosition: Point
    private _orientation: Direction

    get entity() { return this._entity }

    set entity(value) {
        this._entity = value
        this.updateVisual()
    }

    updateVisual() {
        let entity = this._entity
        this.remove(this.children[0])
        this.add(MeshFactory.getMesh(entity))

        if (entity.$type === "PlayerEntity") {
            switch (entity.state.orientation) {
                case Direction.North: this.setRotationFromEuler(new Euler(0, 0, 0)); break
                case Direction.East: this.setRotationFromEuler(new Euler(0, 0, 1.5 * Math.PI)); break
                case Direction.South: this.setRotationFromEuler(new Euler(0, 0, Math.PI)); break
                case Direction.West: this.setRotationFromEuler(new Euler(0, 0, .5 * Math.PI)); break
            }
        } else if (entity.$type === "PistonEntity") {
            switch (entity.state.orientation) {
                case Direction.North: this.setRotationFromEuler(new Euler(0, 0, 0)); break
                case Direction.East: this.setRotationFromEuler(new Euler(0, 0, 1.5 * Math.PI)); break
                case Direction.South: this.setRotationFromEuler(new Euler(0, 0, Math.PI)); break
                case Direction.West: this.setRotationFromEuler(new Euler(0, 0, .5 * Math.PI)); break
            }
        } else {
            this.setRotationFromEuler(new Euler(0, 0, 0))
        }
    }

    get mapPosition() {
        return this._mapPosition
    }

    set mapPosition(position) {
        this._mapPosition = position
    }

    get orientation() {
        return this._orientation
    }

    set orientation(o) {
        this._orientation = o
    }

    constructor(entity: Entity, position: Point) {
        super()
        this.entity = entity
        this.mapPosition = position
    }

    update(deltaTime: number) {
        let targetPosition = new Vector3(this._mapPosition.x, this._mapPosition.y, 1)
        let p = this.position.lerp(targetPosition, 10 * deltaTime)
        this.position.set(p.x, p.y, p.z)

        let targetRotation: number
        switch (this._orientation) {
            case Direction.North: targetRotation = 0; break
            case Direction.East: targetRotation = 1.5 * Math.PI; break
            case Direction.South: targetRotation = Math.PI; break
            case Direction.West: targetRotation = .5 * Math.PI; break
        }

        let rotation = EntityVisual.lerpAngle(this.rotation.z, targetRotation, 10 * deltaTime)
        this.setRotationFromEuler(new Euler(0, 0, rotation))
    }

    private static lerpAngle(a: number, b: number, t: number) {
        let lerp = function (a: number, b: number, t: number) {
            return (1 - t) * a + t * b
        }

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

class GameMapSceneInfo {
    size: Point
    tiles: TileVisual[]
    entities: { key: EntityId, value: EntityVisual }[]

    getTileAt(position: Point) {
        return this.tiles[position.y * this.size.x + position.x]
    }

    getEntity(entityId: EntityId) {
        return this.entities.find(e => e.key.id == entityId.id)
    }

    constructor(initialMap: GameMap) {
        this.size = initialMap.size

        this.tiles = initialMap.tiles.map((tile, i) => new TileVisual(tile,
            new Point(i % initialMap.size.x, ~~(i / initialMap.size.x))))

        this.entities = initialMap.entities.map(e => {
            return {
                key: e.key,
                value: new EntityVisual(e.value.entity, e.value.position)
            }
        })
    }
}

export class GameScene {

    readonly map: GameMap
    readonly mapSceneInfo: GameMapSceneInfo
    readonly scene = new Scene()
    readonly camera: OrthographicCamera
    readonly renderer = new WebGLRenderer()
    readonly clock = new Clock(false)

    constructor(map: GameMap) {

        let audioListener = new AudioListener()

        GameAssets.initialize(audioListener)
        MeshFactory.initialize()

        // init renderer
        this.renderer.setPixelRatio(window.devicePixelRatio)
        document.body.appendChild(this.renderer.domElement)

        // init camera
        this.camera = new OrthographicCamera(0, 0, 0, 0)
        this.camera.position.set(map.size.x / 2 - .5, map.size.y / 2 - .5, 12)
        this.camera.add(audioListener)

        // init lights
        this.scene.add(new AmbientLight(0xffffff, 2))

        // init map
        this.map = map
        this.mapSceneInfo = new GameMapSceneInfo(map)
        this.mapSceneInfo.tiles.forEach(t => this.scene.add(t));
        this.mapSceneInfo.entities.forEach(e => this.scene.add(e.value));

        this.adjustForWindowSize()
        this.clock.start()
        this.runGameLoop()
    }

    private runGameLoop() {
        let deltaTime = this.clock.getDelta()
        this.mapSceneInfo.entities.forEach(kvp => kvp.value.update(deltaTime))
        this.renderer.render(this.scene, this.camera)
        requestAnimationFrame(() => this.runGameLoop())
    }

    adjustForWindowSize() {
        this.renderer.setSize(window.innerWidth, window.innerHeight)
        let aspectRatio = window.innerWidth / window.innerHeight
        let mapAspectRatio = this.map.size.x / this.map.size.y

        if (mapAspectRatio > aspectRatio) {
            this.camera.left = -this.map.size.x / 2;
            this.camera.right = this.map.size.x / 2;
            this.camera.bottom = this.camera.left / aspectRatio
            this.camera.top = this.camera.right / aspectRatio
        } else {
            this.camera.bottom = -this.map.size.y / 2;
            this.camera.top = this.map.size.y / 2;
            this.camera.left = this.camera.bottom * aspectRatio
            this.camera.right = this.camera.top * aspectRatio
        }
        this.camera.updateProjectionMatrix()
    }

    handleEvent(event: GameEvent) {
        switch (event.$type) {
            case "EntityMovedEvent": {
                let entityVisual = this.mapSceneInfo.getEntity(event.props.entityId)
                entityVisual.value.mapPosition = event.props.newPosition
                break
            }

            case "PlayerRotatedEvent": {
                let entityVisual = this.mapSceneInfo.getEntity(event.props.entityId)
                entityVisual.value.orientation = event.props.orientation
                break
            }

            case "GateOpenedEvent":
            case "GateClosedEvent": {
                let tileVisual = this.mapSceneInfo.getTileAt(event.props.position)
                tileVisual.tile.state = event.props.gate
                tileVisual.updateVisual()
                break
            }

            case "BalloonColoredEvent": {
                let entityVisual = this.mapSceneInfo.getEntity(event.props.entityId)
                entityVisual.value.entity.state = event.props.balloon
                entityVisual.value.updateVisual()
                let tileVisual = this.mapSceneInfo.getTileAt(event.props.inkPosition)
                tileVisual.tile = { $type: "PathTile", state: null }
            }

            default:
                console.warn("Unhandled '" + event.$type + "': " + event)
        }
    }

    handleEffects(effects: Effect[]) {
        effects.forEach(async effect => {
            switch (effect.$type) {
                case "TileUpdateEffect": {
                    let tileVisual = this.mapSceneInfo.getTileAt(effect.props.position)
                    tileVisual.tile = effect.props.tile
                    break
                }

                case "EntitySpawnEffect": {
                    let entity = effect.props.entity
                    let position = effect.props.position
                    let visual = new EntityVisual(entity, position)
                    this.mapSceneInfo.entities.push({ key: effect.props.entityId, value: visual })
                    this.scene.add(visual)
                    break
                }

                case "EntityDespawnEffect": {
                    let i = this.mapSceneInfo.entities.findIndex((e, i) => e.key.id === effect.props.entityId.id)
                    if (i != -1) {
                        let visual = this.mapSceneInfo.entities[i]
                        this.mapSceneInfo.entities.splice(i, 1)
                        this.scene.remove(visual.value)
                    }
                    break
                }

                case "EntityUpdateEffect": {
                    let entityVisual = this.mapSceneInfo.getEntity(effect.props.entityId)
                    entityVisual.value.entity = effect.props.entity
                    break
                }

                case "EntityMoveEffect": {
                    let entityVisual = this.mapSceneInfo.getEntity(effect.props.entityId)
                    entityVisual.value.mapPosition = effect.props.newPosition
                    break
                }

                case "SoundEffect": {
                    let sound = await GameAssets.getSoundAsync("click")
                    if (sound)
                        sound.play()
                    break
                }
            }
        })
    }
}