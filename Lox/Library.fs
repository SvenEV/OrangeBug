module LoxServer

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

type LogHub() = inherit Hub()

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

    let print (hub: IHubContext<LogHub>) (text: string) =
        hub.Clients.All.SendAsync("ReceiveLog", text) |> ignore

    let printmd (hub: IHubContext<LogHub>) =
        CommonMarkConverter.Convert >> print hub

    member __.ConfigureServices(services: IServiceCollection) =
        services.AddSignalR() |> ignore

    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment, hub: IHubContext<LogHub>) =
        if env.IsDevelopment() then app.UseDeveloperExceptionPage() |> ignore
        app.UseSignalR(fun routes -> routes.MapHub<LogHub>(PathString("/lox")) |> ignore) |> ignore
        app.Use(serveFile) |> ignore

        Lox.logger <- Some {
            print = print hub
            printmd = printmd hub
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