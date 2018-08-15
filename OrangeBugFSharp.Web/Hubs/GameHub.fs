namespace OrangeBug.Web.Hubs

open Microsoft.AspNetCore.SignalR
open System.Threading.Tasks
open OrangeBug
open OrangeBug.Game
open OrangeBug.Hosting

type GameHub(hub: IHubContext<GameHub>) =
    inherit Hub()

    member this.Join() = async {
        
        //let onSimulationChanged (simulation: Simulation) =
        //    let eventsString =
        //        simulation.scheduledEvents
        //        |> Seq.map (fun ev -> sprintf "%s (time %i, duration %i)" (ev.event.GetType().Name) ev.time.value ev.duration.value)
        //        |> String.concat "\r\n"

        //    let text =
        //        (sprintf "Time: %i\r\n%s" simulation.time.value eventsString)

        //    hub.Clients.All.SendAsync("ReceiveDebugText", text) |> ignore


        let onSignal signal = async {
            match signal with
            | ReceiveInitialMap (map, time, tickTargetTime) ->
                do! Async.AwaitTask(hub.Clients.All.SendAsync("ReceiveInitialMap", map, time.value, tickTargetTime))
            | ReceiveEvents (events, time) ->
                do! Async.AwaitTask(hub.Clients.All.SendAsync("ReceiveEvents", events, time.value))
            | ReceiveDebugText text ->
                do! Async.AwaitTask(hub.Clients.All.SendAsync("ReceiveDebugText", text))
        }

        let newSimulation = Simulation.create SampleMaps.sampleMap1
        SessionManager.create (onSignal >> Async.Start) newSimulation this.Context.ConnectionId |> ignore
    }

    member this.MovePlayer(direction: string) =
        let direction = Direction.tryParse direction

        match direction with
        | None -> ()
        | Some direction ->
            // move player
            MovePlayerIntent { name = "Player"; direction = direction; }
            |> RequestIntent
            |> SessionManager.handleRequest this.Context.ConnectionId

    override this.OnDisconnectedAsync ex =
        SessionManager.kill this.Context.ConnectionId
        Task.CompletedTask

