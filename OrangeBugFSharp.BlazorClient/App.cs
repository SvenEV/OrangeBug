/*
    Configuring this here is temporary. Later we'll move the app config
    into Program.cs, and it won't be necessary to specify AppAssembly.
    <Router AppAssembly=typeof(Program).Assembly />
*/
using System.Threading.Tasks;
using Microsoft.AspNetCore.Blazor;
using Microsoft.AspNetCore.Blazor.Components;
using Microsoft.AspNetCore.Blazor.RenderTree;
using Microsoft.JSInterop;
using Mono.WebAssembly.Interop;
using Newtonsoft.Json;
using OrangeBug;
using OrangeBug.Game;
using OrangeBug.Hosting;

public class App : BlazorComponent
{
    private static readonly JsonConverter[] _converters = new JsonConverter[]
    {
        new Serialization.MapConverter(),
        new Serialization.DiscriminatedUnionConverter(),
        new Serialization.GameTimeConverter(),
        new Serialization.GameTimeSpanConverter()
    };

    private void HandleSignal(Signal signal)
    {

    }

    protected override async Task OnInitAsync()
    {
        var sim = Simulation.create(GameMap.create(20, 10));
        SessionManager.create(HandleSignal, sim, "default");

        //var json = JsonConvert.SerializeObject(map, _converters);
        //await JSRuntime.Current.InvokeAsync<bool>("OrangeBug.onReceiveInitialMap", json, null, 0, 0);
    }

    protected override void BuildRenderTree(RenderTreeBuilder builder)
    {
    }
}