import * as SignalR from "@aspnet/signalr"
import { GameScene } from "./GameScene";
import { GameMap, Direction } from "./CommonTypes";

class GameClient {

    private readonly connection: SignalR.HubConnection    

    private scene: GameScene

    constructor() {
        this.connection = new SignalR.HubConnectionBuilder()
            .withUrl("/game")
            .build()

        this.connection.onclose(() => document.getElementById("arrowControls").style.display = "none")

        this.connection.on("ReceiveDebugMessage", (message: string) => {
            console.info(message)
        })

        this.connection.on("ReceiveInitialMap", (initialMap: GameMap) => {
            this.scene = new GameScene(initialMap)
            console.info(initialMap)
            window.onresize = () => this.scene.adjustForWindowSize()
        })

        this.connection.on("ReceiveEffects", (effects: any[]) => {
            this.scene.handleEffects(effects)
        })

        window.onkeydown = ev => {
            console.info(ev.key)

            switch (ev.key) {
                case "ArrowLeft":
                case "A":
                case "a":
                    this.movePlayer(Direction.West)
                    ev.preventDefault()
                    break

                case "ArrowRight":
                case "D":
                case "d":
                    this.movePlayer(Direction.East)
                    ev.preventDefault()
                    break

                case "ArrowUp":
                case "W":
                case "w":
                    this.movePlayer(Direction.North)
                    ev.preventDefault()
                    break

                case "ArrowDown":
                case "S":
                case "s":
                    this.movePlayer(Direction.South)
                    ev.preventDefault()
                    break
            }
        }

        // connect to server, bootstrapping the game
        this.connection.start()
            .catch((err: any) => console.error(err.toString()))
            .then((_: any) => this.onConnected())
    }

    private onConnected() {
        this.connection.invoke("Join")
    }

    movePlayer(direction: Direction) {
        this.connection.invoke("MovePlayer", direction)    
    }
}

(window as any).OrangeBug = new GameClient()