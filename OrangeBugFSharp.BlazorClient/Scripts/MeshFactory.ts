import { PlaneGeometry, Geometry, Material, Mesh, BoxGeometry, MeshStandardMaterial, MeshBasicMaterial, MeshMaterialType } from "three";
import { AssetLoader } from "./AssetLoader";
import { Tile, Entity } from "./CommonTypes";

export class MeshFactory {

    private static readonly geometries: { [key: string]: Geometry } = {}
    private static readonly materials: { [key: string]: MeshMaterialType } = {}
    private static readonly meshGenerators: { [key: string]: (() => Mesh) } = {}

    static initialize() {
        this.geometries.Plane = new PlaneGeometry(1, 1)
        this.geometries.Cube = new BoxGeometry(1, 1, 1)

        this.materials.Default = new MeshStandardMaterial({ color: 0xff00ff });

        this.meshGenerators.Default = () => new Mesh(this.geometries.Plane, this.getMaterial("NoSprite"))
        this.meshGenerators.PathTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Path"))
        this.meshGenerators.WallTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Wall"))
        this.meshGenerators.ButtonTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Button"))
        this.meshGenerators.TeleporterTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Teleporter"))
        this.meshGenerators.CornerTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Corner"))
        this.meshGenerators.PistonTile = () => new Mesh(this.geometries.Plane, this.getMaterial("Piston"))
        this.meshGenerators.PistonEntity = () => new Mesh(this.geometries.Plane, this.getMaterial("PistonEntity"))
        this.meshGenerators.BoxEntity = () => new Mesh(this.geometries.Plane, this.getMaterial("Box"))
        this.meshGenerators.PlayerEntity = () => new Mesh(this.geometries.Plane, this.getMaterial("Player"))
    }

    private static getMaterial(spriteKey: string) {
        let material = this.materials[spriteKey]

        if (material)
            return material

        material = new MeshBasicMaterial({ map: AssetLoader.getSprite(spriteKey), transparent: true });
        this.materials[spriteKey] = material
        return material
    }

    static getMesh(tileOrEntity: Tile | Entity): Mesh {
        switch (tileOrEntity.$type) {
            // special cases first (mesh depending on tile/entity state)
            case "GateTile": {
                return new Mesh(this.geometries.Plane, this.getMaterial(tileOrEntity.state.isOpen
                    ? "GateOpened"
                    : "GateClosed"))
            }

            case "InkTile": {
                return new Mesh(this.geometries.Plane, this.getMaterial("Ink" + tileOrEntity.state.color))
            }

            case "PinTile": {
                return new Mesh(this.geometries.Plane, this.getMaterial("Pin" + tileOrEntity.state.color))
            }

            case "BalloonEntity": {
                return new Mesh(this.geometries.Plane, this.getMaterial("Balloon" + tileOrEntity.state.color))
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