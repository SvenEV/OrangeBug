import * as Three from "three"
import { Camera, Scene, PerspectiveCamera, WebGLRenderer, BoxGeometry, MeshBasicMaterial, Mesh, TextureLoader, Texture, Audio, AudioLoader, PlaneGeometry, MeshStandardMaterial, DirectionalLight, WebGLShadowMap, ShadowMapType, PCFSoftShadowMap, DirectionalLightHelper, AmbientLight, Object3D, Geometry, Material, AudioListener, AudioBuffer, CylinderGeometry, OrthographicCamera } from "three"
import { Tile, Entity, Effect, Point, Direction, GameMap } from "./CommonTypes"

class GameAssets {

    private static readonly textureLoader = new TextureLoader()
    private static readonly audioLoader = new AudioLoader()

    static readonly sprites: { [key: string]: Texture } = {
        "PlayerRight": null,
        "Path": null,
        "Wall": null,
        "Box": null,
        "NoSprite": null,
        "Button": null,
        "DummyWall": null,
    }

    static readonly sounds: { [key: string]: Audio } = {
        "click": null,
        "boxscrape": null
    }

    static initialize(audioListener: AudioListener) {
        for (let key in this.sprites) {
            let texture = this.textureLoader.load("images/sprites/" + key + ".png")
            this.sprites[key] = texture
        }

        for (let key in this.sounds) {
            this.audioLoader.load("sounds/" + key + ".mp3", (buffer: AudioBuffer) => {
                let audio = new Audio(audioListener)
                audio.setBuffer(buffer)
                this.sounds[key] = audio;
            }, null, null)
        }
    }
}

class MeshFactory {

    private static geometries: { [key: string]: Geometry } = {}
    private static materials: { [key: string]: Material } = {}
    private static meshGenerators: { [key: string]: (() => Mesh) } = {}

    static initialize() {
        this.geometries.Plane = new PlaneGeometry(1, 1)
        this.geometries.Cube = new BoxGeometry(1, 1, 1)

        this.materials.Default = new MeshStandardMaterial({ color: 0xff00ff });
        this.materials.Path = new MeshStandardMaterial({ map: GameAssets.sprites["Path"] });
        this.materials.Wall = new MeshStandardMaterial({ map: GameAssets.sprites["Wall"] });
        this.materials.Box = new MeshStandardMaterial({ map: GameAssets.sprites["Box"], transparent: true });
        this.materials.PlayerRight = new MeshStandardMaterial({ map: GameAssets.sprites["PlayerRight"], transparent: true });

        this.meshGenerators.Default = () => new Mesh(this.geometries.Plane, this.materials.Default)
        this.meshGenerators.PathTile = () => new Mesh(this.geometries.Plane, this.materials.Path)
        this.meshGenerators.WallTile = () => new Mesh(this.geometries.Plane, this.materials.Wall)
        this.meshGenerators.BoxEntity = () => new Mesh(this.geometries.Plane, this.materials.Box)
        this.meshGenerators.PlayerEntity = () => new Mesh(this.geometries.Plane, this.materials.PlayerRight)
    }

    static getMesh(tileOrEntity: Tile | Entity): Mesh {
        let meshGenerator = this.meshGenerators[tileOrEntity.$type]
        if (!meshGenerator)
            meshGenerator = this.meshGenerators.Default

        let mesh = meshGenerator()
        mesh.castShadow = true
        mesh.receiveShadow = true
        return mesh;
    }
}

class TileVisual extends Object3D {
    private _tile: Tile

    get tile() { return this._tile }

    set tile(value) {
        this.remove(this.children[0])
        this.add(MeshFactory.getMesh(value))
        this._tile = value
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

    get entity() { return this._entity }

    set entity(value) {
        this.remove(this.children[0])
        this.add(MeshFactory.getMesh(value))
        this._entity = value
    }

    get mapPosition() {
        return { x: this.position.x, y: this.position.y }
    }

    set mapPosition(position) {
        this.position.set(position.x, position.y, 1)
    }

    constructor(entity: Entity, position: Point) {
        super()
        this.entity = entity
        this.mapPosition = position
    }
}

class GameMapSceneInfo {
    size: Point
    tiles: TileVisual[]
    entities: { key: Point, value: EntityVisual }[]

    getTileAt(position: Point) {
        return this.tiles[position.y * this.size.x + position.x]
    }

    getEntityAt(position: Point) {
        return this.entities.find(e =>
            e.key.x === position.x &&
            e.key.y === position.y)
    }

    constructor(initialMap: GameMap) {
        this.size = initialMap.size

        this.tiles = initialMap.tiles.map((tile, i) => new TileVisual(tile,
            new Point(i % initialMap.size.x, ~~(i / initialMap.size.x)))),

        this.entities = initialMap.entities.map(e => {
            return {
                key: e.key,
                value: new EntityVisual(e.value, e.key)
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

    constructor(map: GameMap) {

        let audioListener = new AudioListener()

        GameAssets.initialize(audioListener)
        MeshFactory.initialize()
        
        // init renderer
        this.renderer.setPixelRatio(window.devicePixelRatio)
        this.renderer.shadowMap.enabled = true
        this.renderer.shadowMap.type = PCFSoftShadowMap // default THREE.PCFShadowMap
        document.body.appendChild(this.renderer.domElement)

        // init camera
        this.camera = new OrthographicCamera(0, 0, 0, 0)
        this.camera.position.set(map.size.x / 2 - .5, map.size.y / 2 - .5, 12)
        this.camera.add(audioListener)

        // init lights
        let light = new DirectionalLight(0xffffff, .5)
        light.position.z = 3
        light.castShadow = true
        light.target.position.set(2, 0, 0)
        this.scene.add(light)
        this.scene.add(light.target)
        this.scene.add(new AmbientLight(0xffffff, .6))

        // init map
        this.map = map
        this.mapSceneInfo = new GameMapSceneInfo(map)
        this.mapSceneInfo.tiles.forEach(t => this.scene.add(t));
        this.mapSceneInfo.entities.forEach(e => this.scene.add(e.value));

        this.adjustForWindowSize()
        this.runGameLoop()
    }

    private runGameLoop() {
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

    handleEffects(effects: Effect[]) {
        effects.forEach(effect => {
            switch (effect["$type"]) {
                case "TileUpdateEffect":
                    let tileVisual = this.mapSceneInfo.getTileAt(effect.props.position)
                    tileVisual.tile = effect.props.tile
                    break

                case "EntityUpdateEffect":
                    let entityVisual = this.mapSceneInfo.getEntityAt(effect.props.position)    
                    entityVisual.value.entity = effect.props.entity    
                    break

                case "EntityMoveEffect":
                    let entityVisual2 = this.mapSceneInfo.getEntityAt(effect.props.sourcePosition)
                    entityVisual2.key = effect.props.targetPosition
                    entityVisual2.value.mapPosition = effect.props.targetPosition
                    break

                case "SoundEffect":
                    let sound = GameAssets.sounds["click"]
                    sound.play()
                    break
            }
        }) 
    }
}