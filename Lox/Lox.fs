module LoxLib.LoxServer

open System
open System.Diagnostics
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.SignalR
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Newtonsoft.Json
open Serialization
open System.Collections.Generic

let mutable globalCommands = []
let messages = List<LogMessage>()

let sendMessage (client: IClientProxy) id message =
    let dto = Dto.mapMessage message id
    client.SendAsync("ReceiveLogMessage", dto) |> ignore

let sendGlobalCommands (client: IClientProxy) commands =
    let dtos = commands |> List.map Dto.mapCommand
    client.SendAsync("ReceiveGlobalCommands", dtos) |> ignore

type LogHub() =
    inherit Hub()

    override hub.OnConnectedAsync() =
        sendGlobalCommands (hub.Clients.Caller) globalCommands
        messages |> Seq.iteri (sendMessage (hub.Clients.Caller))
        Task.CompletedTask

    member __.InvokeCommand(messageId: int, label: string) =
        match messageId with
        | -1 ->
            // Find matching global command and invoke it
            match globalCommands |> Seq.tryFind (fun cmd -> cmd.label = label) with
            | Some command -> command.action()
            | None -> ()
        | id ->
            // Find message, find matching command and invoke it
            if id < messages.Count then
                let msg = messages.[id]
                let command =
                    msg.elements
                    |> Seq.collect (function LogCommandBar cmds -> cmds | _ -> [])
                    |> Seq.tryFind (fun cmd -> cmd.label = label)
                match command with
                | Some cmd -> cmd.action()
                | None -> ()

let log (hub: IHubContext<LogHub>) message =
    messages.Add message
    sendMessage (hub.Clients.All) (messages.Count - 1) message

let setGlobalCommands (hub: IHubContext<LogHub>) commands =
    globalCommands <- commands
    sendGlobalCommands (hub.Clients.All) commands

type Startup() =
    let serveFile (context: HttpContext) (next: Func<Task>) =
        let filename =
            if context.Request.Path.Value = "/"
            then "index.html"
            else context.Request.Path.Value.TrimStart([| '/' |])
        let file = typedefof<LogHub>.Assembly.GetManifestResourceStream("Lox.wwwroot." + filename)
        match file with
        | null -> next.Invoke()
        | file -> file.CopyToAsync(context.Response.Body)

    member __.ConfigureServices(services: IServiceCollection) =
        services.AddSignalR()
            .AddJsonProtocol(fun options ->
                options.PayloadSerializerSettings.Converters.Add(MapConverter())
                options.PayloadSerializerSettings.Converters.Add(DiscriminatedUnionConverter())) |> ignore
        ()

    member __.Configure(app: IApplicationBuilder, env: IHostingEnvironment, hub: IHubContext<LogHub>) =
        if env.IsDevelopment() then app.UseDeveloperExceptionPage() |> ignore
        app.UseSignalR(fun routes -> routes.MapHub<LogHub>(PathString("/lox")) |> ignore) |> ignore
        app.Use(serveFile) |> ignore

        Lox.logger <- Some {
            log = log hub
            setCommands = setGlobalCommands hub
        }
        ()

let init () =
    match Lox.logger with
    | Some _ -> ()
    | None ->
        let config = ConfigurationBuilder().Build()
        WebHostBuilder()
            .UseConfiguration(config)
            .ConfigureLogging(fun o -> o.AddConsole() |> ignore)
            .UseKestrel()
            .UseEnvironment("Development")
            .UseStartup<Startup>()
            .Build()
            .RunAsync() |> ignore

let launchBrowser () =
    Process.Start(ProcessStartInfo("http://localhost:5000/", UseShellExecute = true)) |> ignore