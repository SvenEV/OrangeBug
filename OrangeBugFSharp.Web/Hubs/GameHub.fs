namespace OrangeBug.Web.Hubs

open Microsoft.AspNetCore.SignalR
open System.Threading.Tasks
open OrangeBug
open OrangeBug.Web
open OrangeBug.Game

type GameHub(hub: IHubContext<GameHub>) =
    inherit Hub()

    member this.Join() = async {
        
        let onEvents (events: Event list, time: GameTime) =
            hub.Clients.All.SendAsync("ReceiveEvents", events) |> ignore

        let onSimulationChanged (simulation: Simulation) =
            hub.Clients.All.SendAsync("ReceiveTick", simulation.time.value) |> ignore

        let newSimulation = Simulation.create (SampleMaps.createInitialMap())
        SessionManager.createSession this.Context.ConnectionId newSimulation onSimulationChanged onEvents |> ignore
        do! Async.AwaitTask(this.Clients.Caller.SendAsync("ReceiveInitialMap", newSimulation.map))
    }

    member this.MovePlayer(direction: string) = async {
        let session = SessionManager.getSession this.Context.ConnectionId
        let direction = Direction.tryParse direction

        match direction with
        | None -> ()
        | Some direction ->
            // move player
            let intent = MovePlayerIntent { name = "Player"; direction = direction }
            session.clock.queueIntent intent
    }

    override this.OnDisconnectedAsync ex =
        SessionManager.killSession this.Context.ConnectionId
        Task.CompletedTask

