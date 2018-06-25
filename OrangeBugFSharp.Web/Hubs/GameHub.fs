namespace OrangeBugFSharp.Web.Hubs

open Microsoft.AspNetCore.SignalR
open OrangeBug.GameMap
open OrangeBug
open OrangeBug.TilesEntities
open OrangeBug.IntentsEvents
open System.Threading.Tasks
open OrangeBug.Web

type GameHub() =
    inherit Hub()
    
    member this.Join() = async {
        let session = SessionManager.getOrCreateSession this.Context.ConnectionId
        do! Async.AwaitTask(this.Clients.Caller.SendAsync("ReceiveInitialMap", session.map))
    }

    member this.MovePlayer(direction: string) = async {
        let session = SessionManager.getOrCreateSession this.Context.ConnectionId
        let direction = Direction.tryParse direction

        match direction with
        | None -> ()
        | Some direction ->
            let intent = MovePlayerIntent { name = "Player"; direction = direction }
            let result = Gameplay.handleIntent session.map.accessor intent
            let effects = List.collect (Game.eventToEffects session.map) result.events
            session.map.applyEffects effects
            do! Async.AwaitTask(this.Clients.Caller.SendAsync("ReceiveEffects", effects))
    }

    override this.OnDisconnectedAsync ex =
        SessionManager.killSession this.Context.ConnectionId
        Task.CompletedTask

