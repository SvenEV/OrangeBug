namespace OrangeBug.Web.Hubs

open Microsoft.AspNetCore.SignalR
open System.Threading.Tasks
open OrangeBug
open OrangeBug.Web
open OrangeBug.Game

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
            match Gameplay.processIntent intent session.map with
            | Rejected _ -> ()
            | Accepted events ->
                let effects = events |> Seq.collect Effect.eventToEffects
                session.map <- effects |> Seq.fold GameMap.applyEffect session.map
                do! Async.AwaitTask(this.Clients.Caller.SendAsync("ReceiveEffects", effects))
    }

    override this.OnDisconnectedAsync ex =
        SessionManager.killSession this.Context.ConnectionId
        Task.CompletedTask

