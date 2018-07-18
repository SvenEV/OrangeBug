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
        do! Async.AwaitTask(this.Clients.Caller.SendAsync("ReceiveInitialMap", session.simulation.map))
    }

    member this.MovePlayer(direction: string) = async {
        let session = SessionManager.getOrCreateSession this.Context.ConnectionId
        let direction = Direction.tryParse direction

        match direction with
        | None -> ()
        | Some direction ->
            // move player
            let intent = MovePlayerIntent { name = "Player"; direction = direction }
            session.simulation <- Simulation.processIntent intent session.simulation
            // advance simulation by 1 tick
            let newSim, events = Simulation.advance session.simulation
            session.simulation <- newSim
            do! Async.AwaitTask(this.Clients.Caller.SendAsync("ReceiveEvents", events))
    }

    override this.OnDisconnectedAsync ex =
        SessionManager.killSession this.Context.ConnectionId
        Task.CompletedTask

