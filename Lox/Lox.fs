module LoxLib.LoxServer

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.SignalR
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open System
open System.Diagnostics
open System.Threading.Tasks
open CommonMark
open Lox
open Newtonsoft.Json
open Serialization

let mutable debugCommands : LoxCommand list = []

type LogMessage = {
    elements: LogElement list
    category: string
    time: string
}

type LogHub() =
    inherit Hub()
    override hub.OnConnectedAsync() =
        hub.Clients.Caller.SendAsync("ReceiveCommands", debugCommands)
    member __.InvokeCommand(label: string) =
        match debugCommands |> Seq.tryFind (fun cmd -> cmd.label = label) with
        | None -> ()
        | Some command -> command.action()

let log (hub: IHubContext<LogHub>) (elements: LogElement list) =
    let message = {
        elements = elements
        category = "General"
        time = DateTimeOffset.Now.ToString("yyyy-MM-dd hh:mm:ss")
    }
    hub.Clients.All.SendAsync("ReceiveLog", message) |> ignore

let setCommands (hub: IHubContext<LogHub>) (commands: LoxCommand list) =
    debugCommands <- commands
    hub.Clients.All.SendAsync("ReceiveCommands", commands) |> ignore

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
                options.PayloadSerializerSettings.Converters.Add(ImageConverter())
                options.PayloadSerializerSettings.Converters.Add(MapConverter())
                options.PayloadSerializerSettings.Converters.Add(DiscriminatedUnionConverter())) |> ignore
        ()

    member __.Configure(app: IApplicationBuilder, env: IHostingEnvironment, hub: IHubContext<LogHub>) =
        if env.IsDevelopment() then app.UseDeveloperExceptionPage() |> ignore
        app.UseSignalR(fun routes -> routes.MapHub<LogHub>(PathString("/lox")) |> ignore) |> ignore
        app.Use(serveFile) |> ignore

        Lox.logger <- Some {
            log = log hub
            setCommands = setCommands hub
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