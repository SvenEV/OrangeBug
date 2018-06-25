namespace OrangeBugFSharp.Web

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open OrangeBugFSharp.Web.Hubs
open Newtonsoft.Json
open OrangeBug.Serialization

type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        // Add framework services.
        services.AddSignalR().AddJsonProtocol(fun options ->
            options.PayloadSerializerSettings <-
                JsonSerializerSettings(Converters = [| MapConverter(); DiscriminatedUnionConverter() |])) |> ignore

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        app.UseStaticFiles() |> ignore
        app.UseDefaultFiles() |> ignore
        app.UseSignalR(fun hubs -> hubs.MapHub<GameHub>(PathString "/game")) |> ignore

    member val Configuration : IConfiguration = null with get, set