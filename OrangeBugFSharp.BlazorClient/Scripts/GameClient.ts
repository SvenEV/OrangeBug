﻿import * as SignalR from "@aspnet/signalr"
import { GameScene } from "./GameScene";
import { GameMap, Direction, ScheduledEvent } from "./CommonTypes";

class GameClient {

    private readonly connection: SignalR.HubConnection    

    private scene: GameScene

    constructor() {
        /*this.connection = new SignalR.HubConnectionBuilder()
            .withUrl("/game")
            .build()

        this.connection.onclose(() => this.onDisconnected())

        this.connection.on("ReceiveInitialMap", (initialMap: GameMap, initialTime: number, tickTargetTime: number) =>
            this.onReceiveInitialMap(initialMap, initialTime, tickTargetTime))

        this.connection.on("ReceiveEvents", (events: ScheduledEvent[], time: number) =>
            this.onReceiveEvents(events, time))

        this.connection.on("ReceiveDebugText", (text: string) =>
            this.onReceiveDebugText(text))*/

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
        /*this.connection.start()
            .catch((err: any) => console.error(err.toString()))
            .then((_: any) => this.onConnected())*/
    }

    onConnected() {
        this.connection.invoke("Join")
    }

    onDisconnected() {
        document.getElementById("arrowControls").style.display = "none"
    }

    onSignal(json: string) {
        let signal = JSON.parse(json)
        let client = (window as any).OrangeBug as GameClient

        switch (signal.$type) {
            case "ReceiveInitialMap":
                client.onReceiveInitialMap(signal.map, signal.time, signal.tickTargetTime)
                break

            case "ReceiveEvents":
                client.onReceiveEvents(signal.events, signal.time)
                break

            case "ReceiveDebugText":
                client.onReceiveDebugText(signal.text)
                break
        }
    }

    onReceiveInitialMap(initialMap: GameMap, initialTime: number, tickTargetTime: number) {
        this.scene = new GameScene(initialMap, initialTime, tickTargetTime)
        console.info(initialMap)
        window.onresize = () => this.scene.adjustForWindowSize()
    }

    onReceiveEvents(events: ScheduledEvent[], time: number) {
        // remember, events.forEach(this.scene.handleEvent) doesn't work for some reason
        events.forEach(e => this.scene.handleEvent(e, time))
    }

    onReceiveDebugText(text: string) {
        this.scene.debugText.innerText = text
    }

    movePlayer(direction: Direction) {
        //this.connection.invoke("MovePlayer", direction)
        (window as any).DotNet.invokeMethod("OrangeBugFSharp.BlazorClient", "RequestMovePlayer", direction.toString())
    }
}

(window as any).OrangeBug = new GameClient()