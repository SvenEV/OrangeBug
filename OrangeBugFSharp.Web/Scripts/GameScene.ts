import * as Three from "three"
import { Camera, Scene, PerspectiveCamera, WebGLRenderer, BoxGeometry, MeshBasicMaterial, Mesh, TextureLoader, Texture, Audio, AudioLoader, PlaneGeometry, MeshStandardMaterial, DirectionalLight, WebGLShadowMap, ShadowMapType, PCFSoftShadowMap, DirectionalLightHelper, AmbientLight, Object3D, Geometry, Material, AudioListener, AudioBuffer, CylinderGeometry, OrthographicCamera, Euler } from "three"
import { Tile, Entity, Effect, Point, Direction, GameMap, EntityId } from "./CommonTypes"

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
        this.meshGenerators.GateTile = () => new Mesh(this.geometries.Plane, this.getMaterial("DummyWall"))
        this.meshGenerators.BoxEntity = () => new Mesh(this.geometries.Plane, this.getMaterial("Box"))
        this.meshGenerators.PlayerEntity = () => new Mesh(this.geometries.Plane, this.getMaterial("PlayerRight"))
    }

    private static getMaterial(spriteKey: string) {
        let material = this.materials[spriteKey]

        if (material)
            return material

        material = new MeshStandardMaterial({ map: GameAssets.getSprite(spriteKey), transparent: true });
        this.materials[spriteKey] = material
        return material
    }

    static getMesh(tileOrEntity: Tile | Entity): Mesh {
        switch (tileOrEntity.$type) {
            // special cases first (mesh depending on tile/entity state)
            case "GateTile":
                return new Mesh(this.geometries.Plane, this.getMaterial(tileOrEntity.state.isOpen
                    ? "DummyWallRemoved"
                    : "DummyWall"))

            // simple cases (mesh only depending on tile/entity type)
            default:
                let meshGenerator = this.meshGenerators[tileOrEntity.$type]
                if (!meshGenerator)
                    meshGenerator = this.meshGenerators.Default

                let mesh = meshGenerator()
                mesh.castShadow = true
                mesh.receiveShadow = true
                return mesh;
        }
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

        if (value.$type === "PlayerEntity") {
            switch (value.state.orientation) {
                case "North": this.setRotationFromEuler(new Euler(0, 0, 0)); break
                case "East": this.setRotationFromEuler(new Euler(0, 0, 1.5 * Math.PI)); break
                case "South": this.setRotationFromEuler(new Euler(0, 0, Math.PI)); break
                case "West": this.setRotationFromEuler(new Euler(0, 0, .5 * Math.PI)); break
            }
        }
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
            new Point(i % initialMap.size.x, ~~(i / initialMap.size.x)))),

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
        effects.forEach(async effect => {
            switch (effect["$type"]) {
                case "TileUpdateEffect":
                    let tileVisual = this.mapSceneInfo.getTileAt(effect.props.position)
                    tileVisual.tile = effect.props.tile
                    break

                case "EntityUpdateEffect":
                    let entityVisual = this.mapSceneInfo.getEntity(effect.props.entityId)
                    entityVisual.value.entity = effect.props.entity    
                    break

                case "EntityMoveEffect":
                    let entityVisual2 = this.mapSceneInfo.getEntity(effect.props.entityId)
                    entityVisual2.value.mapPosition = effect.props.newPosition
                    break

                case "SoundEffect":
                    let sound = await GameAssets.getSoundAsync("click")
                    if (sound)
                        sound.play()
                    break
            }
        }) 
    }
}