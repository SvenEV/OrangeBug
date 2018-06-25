import * as SignalR from "@aspnet/signalr"

type SpriteCollection = { [key: string]: HTMLImageElement }
type SoundCollection = { [key: string]: HTMLAudioElement }

type Tile = { $type: string, [key: string]: any }
type Entity = { $type: string, [key: string]: any }
type Point = { x: number, y: number }
enum Direction { North, East, South, West }

type GameMap = {
    size: Point,
    tiles: [Tile],
    entities: [{ key: Point, value: Entity }]
}

class GameClient {

    constructor(canvas: HTMLCanvasElement) {
        this.canvas = canvas;
        this.connection = new SignalR.HubConnectionBuilder()
            .withUrl("/game")
            .build();

        this.connection.onclose(() => document.getElementById("arrowControls").style.display = "none");

        this.connection.on("ReceiveDebugMessage", (message: string) => {
            console.info(message);
        });

        this.connection.on("ReceiveInitialMap", (initialMap: GameMap) => {
            console.info(initialMap);
            this.map = initialMap;
            this.runGameLoop();
        });

        this.connection.on("ReceiveEffects", (effects: any[]) => {
            effects.forEach(effect => {
                switch (effect["$type"]) {
                    case "EntityUpdateEffect":
                        let entityToUpdate = this.getEntityAt(effect.props.position);
                        entityToUpdate.value = effect.props.entity;
                        break;

                    case "EntityMoveEffect":
                        let entityToMove = this.getEntityAt(effect.props.sourcePosition);
                        entityToMove.key = effect.props.targetPosition;
                        break;

                    case "SoundEffect":
                        let sound = this.sounds["click"];
                        sound.play();
                }
            });
        });

        window.onkeydown = ev => {
            console.info(ev.key);

            switch (ev.key) {
                case "ArrowLeft":
                case "A":
                case "a":
                    this.movePlayer(Direction.West);
                    ev.preventDefault();
                    break;

                case "ArrowRight":
                case "D":
                case "d":
                    this.movePlayer(Direction.East);
                    ev.preventDefault();
                    break;

                case "ArrowUp":
                case "W":
                case "w":
                    this.movePlayer(Direction.North);
                    ev.preventDefault();
                    break;

                case "ArrowDown":
                case "S":
                case "s":
                    this.movePlayer(Direction.South);
                    ev.preventDefault();
                    break;
            }
        }

        // connect to server, bootstrapping the game
        this.connection.start()
            .catch((err: any) => console.error(err.toString()))
            .then((_: any) => this.onConnected());
    }

    private readonly connection: SignalR.HubConnection;
    private readonly canvas: HTMLCanvasElement;

    private readonly sprites: SpriteCollection = {
        "PlayerRight": null,
        "Path": null,
        "Wall": null,
        "Box": null,
        "NoSprite": null,
        "Button": null,
        "DummyWall": null,
    };

    private readonly sounds: SoundCollection = {
        "click": null,
        "boxscrape": null
    };

    private map: GameMap;

    private async onConnected() {
        this.connection.invoke("Join");

        for (let key in this.sprites) {
            this.sprites[key] = await this.loadImage("images/sprites/" + key + ".png");
        }

        for (let key in this.sounds) {
            this.sounds[key] = await this.loadSound("sounds/" + key + ".mp3");
        }
    }

    private loadImage(url: string): Promise<HTMLImageElement> {
        return new Promise((resolve, reject) => {
            let image = new Image();
            image.onload = () => resolve(image);
            image.onerror = reject;
            image.src = url;
        });
    }

    private loadSound(url: string): Promise<HTMLAudioElement> {
        return new Promise((resolve, reject) => {
            let audio = new Audio();
            audio.oncanplaythrough = () => resolve(audio);
            audio.onerror = reject;
            audio.src = url;
            document.body.appendChild(audio);
            resolve(audio); // 'oncanplaythrough' not triggered in Edge
        });
    }

    private runGameLoop() {
        this.render();
        requestAnimationFrame(() => this.runGameLoop());
    }

    render() {
        // prepare canvas
        let bounds = this.canvas.getBoundingClientRect();
        this.canvas.width = bounds.width * window.devicePixelRatio;
        this.canvas.height = bounds.height * window.devicePixelRatio;
        let context = this.canvas.getContext("2d");
        context.scale(window.devicePixelRatio, window.devicePixelRatio);
        context.clearRect(0, 0, bounds.width, bounds.height);

        const spriteSize = Math.min(
            bounds.width / this.map.size.x,
            bounds.height / this.map.size.y);

        // draw tiles
        for (let y = 0; y < this.map.size.y; y++) {
            for (let x = 0; x < this.map.size.x; x++) {
                let i = y * this.map.size.x + x;
                let sprite = this.getSprite(this.map.tiles[i]);
                context.drawImage(sprite, x * spriteSize, (this.map.size.y - y - 1) * spriteSize, spriteSize, spriteSize);
            }
        }

        // draw entities
        this.map.entities.forEach(kvp => {
            let sprite = this.getSprite(kvp.value);
            context.drawImage(sprite, kvp.key.x * spriteSize, (this.map.size.y - kvp.key.y - 1) * spriteSize, spriteSize, spriteSize);
        });
    }

    getSprite(tileOrEntity: Tile | Entity) {
        const typeToSpriteMap: { [key: string]: string } = {
            // Tiles
            "PathTile": "Path",
            "WallTile": "Wall",
            "InkTile": "RedInk",
            "PinTile": "RedPool",
            "ButtonTile": "Button",
            "GateTile": "DummyWall",
            // Entities
            "PlayerEntity": "PlayerRight",
            "BoxEntity": "Box",
            "BalloonEntity": "RedBall",
        };

        let type = tileOrEntity.$type
        let sprite = this.sprites[typeToSpriteMap[type]];

        if (!sprite) {
            console.warn("No sprite for tile/entity type '" + type + "'");
            return this.sprites.NoSprite;
        }

        return sprite;
    }

    private getEntityAt(position: Point) {
        return this.map.entities.find(e =>
            e.key.x === position.x &&
            e.key.y === position.y)
    }

    movePlayer(direction: Direction) {
        this.connection.invoke("MovePlayer", direction);
    }
}

(window as any).OrangeBug = new GameClient(document.getElementById("gameCanvas") as HTMLCanvasElement);