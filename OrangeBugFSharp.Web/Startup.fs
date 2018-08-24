namespace OrangeBug.Web

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Newtonsoft.Json
open OrangeBug.Serialization
open OrangeBug.Web.Hubs
open Microsoft.Extensions.FileProviders
open System.IO
open LoxLib.Serialization

type Startup private () =
    new (configuration: IConfiguration) as this =
        Startup() then
        this.Configuration <- configuration

    // This method gets called by the runtime. Use this method to add services to the container.
    member this.ConfigureServices(services: IServiceCollection) =
        // Add framework services.
        services.AddSignalR().AddJsonProtocol(fun options ->
            options.PayloadSerializerSettings <-
                JsonSerializerSettings(
                    Converters = [|
                        MapConverter()
                        SimTimeConverter()
                        SimTimeSpanConverter()
                        DiscriminatedUnionConverter()
                    |])) |> ignore

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        let assetsFolder =
            let path = Path.Combine(Directory.GetCurrentDirectory(), "..", "Assets")
            StaticFileOptions(FileProvider = new PhysicalFileProvider(path))

        app.UseDefaultFiles() |> ignore
        app.UseStaticFiles() |> ignore
        app.UseStaticFiles(assetsFolder) |> ignore            
        app.UseSignalR(fun hubs -> hubs.MapHub<GameHub>(PathString "/game")) |> ignore

    member val Configuration : IConfiguration = null with get, set