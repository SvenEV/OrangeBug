import { Scene, WebGLRenderer, AmbientLight, AudioListener, OrthographicCamera, Clock, Vector3 } from "three"
import { Point, GameMap, EntityId, GameEvent } from "./CommonTypes"
import { EntityVisual } from "./EntityVisual";
import { TileVisual } from "./TileVisual";
import { MeshFactory } from "./MeshFactory";
import { AssetLoader } from "./AssetLoader";

class GameMapSceneInfo {
    readonly size: Point
    readonly tiles: TileVisual[]
    readonly entities: Map<number, EntityVisual>

    getTileAt(position: Point) {
        return this.tiles[position.y * this.size.x + position.x]
    }

    getEntity(entityId: EntityId) {
        return this.entities.get(entityId.id)
    }

    constructor(initialMap: GameMap) {
        this.size = initialMap.size

        this.tiles = initialMap.tiles.map((tile, i) => new TileVisual(tile,
            new Point(i % initialMap.size.x, ~~(i / initialMap.size.x))))

        this.entities = new Map(
            initialMap.entities.map<[number, EntityVisual]>(e =>
                [e.key.id, new EntityVisual(e.value.entity, e.value.position)]))
    }
}

export class GameScene {

    readonly map: GameMap
    readonly mapSceneInfo: GameMapSceneInfo
    readonly tickTargetTime: number
    time: number

    readonly scene = new Scene()
    readonly camera: OrthographicCamera
    readonly renderer = new WebGLRenderer()
    readonly clock = new Clock(false)
    readonly debugText = document.getElementById("debugText")

    constructor(map: GameMap, initialTime: number, tickTargetTime: number) {
        let audioListener = new AudioListener()
        AssetLoader.initialize(audioListener)
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
        this.tickTargetTime = tickTargetTime
        this.time = initialTime
        this.map = map
        this.mapSceneInfo = new GameMapSceneInfo(map)
        this.mapSceneInfo.tiles.forEach(t => this.scene.add(t));
        this.mapSceneInfo.entities.forEach(e => this.scene.add(e));

        this.adjustForWindowSize()
        this.clock.start()
        this.runGameLoop()
    }

    private runGameLoop() {        
        let deltaTime = this.clock.getDelta()
        this.time = this.time + (deltaTime / this.tickTargetTime)
        this.mapSceneInfo.entities.forEach(visual => visual.update(this.time, deltaTime))
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

    handleEvent(event: GameEvent, time: number) {
        this.time = time // sync time with server

        switch (event.$type) {
            case "EntityMovedEvent": {
                let entityVisual = this.mapSceneInfo.getEntity(event.props.entityId)
                let p = event.props.newPosition
                entityVisual.moveAnimation = {
                    startPosition: entityVisual.position.clone(),
                    endPosition: new Vector3(p.x, p.y, 1),
                    startTime: time,
                    duration: 2
                }
                break
            }

            case "PlayerRotatedEvent": {
                let entityVisual = this.mapSceneInfo.getEntity(event.props.entityId)
                entityVisual.orientation = event.props.orientation
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
                entityVisual.entity.state = event.props.balloon
                entityVisual.updateVisual()
                let tileVisual = this.mapSceneInfo.getTileAt(event.props.inkPosition)
                tileVisual.tile = { $type: "PathTile", state: null }
                break
            }

            case "BalloonPoppedEvent": {
                let entityVisual = this.mapSceneInfo.getEntity(event.props.entityId)
                this.mapSceneInfo.entities.delete(event.props.entityId)
                this.scene.remove(entityVisual)
                break
            }

            default:
                console.warn("Unhandled '" + event.$type + "': " + event)
                break
        }
    }
}