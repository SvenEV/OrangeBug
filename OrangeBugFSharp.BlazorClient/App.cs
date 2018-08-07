/*
    Configuring this here is temporary. Later we'll move the app config
    into Program.cs, and it won't be necessary to specify AppAssembly.
    <Router AppAssembly=typeof(Program).Assembly />
*/
using System.Threading.Tasks;
using Microsoft.AspNetCore.Blazor.Components;
using Microsoft.AspNetCore.Blazor.RenderTree;
using Microsoft.FSharp.Core;
using Microsoft.JSInterop;
using Newtonsoft.Json;
using OrangeBug;
using OrangeBug.Game;
using OrangeBug.Hosting;

public class App : BlazorComponent
{
    private static readonly JsonConverter[] _converters = new JsonConverter[]
    {
        new Serialization.MapConverter(),
        new Serialization.GameTimeConverter(),
        new Serialization.GameTimeSpanConverter(),
        new Serialization.DiscriminatedUnionConverter()
    };

    private async void HandleSignal(Signal signal)
    {
        var json = JsonConvert.SerializeObject(signal, _converters);
        await JSRuntime.Current.InvokeAsync<bool>("OrangeBug.onSignal", json, null, 0, 0);
    }

    protected override async Task OnInitAsync()
    {
        var sim = SimulationModule.create(SampleMaps.createInitialMap());
        SessionManager.create(FuncConvert.FromAction<Signal>(HandleSignal), sim, "default");
    }

    [JSInvokable]
    public static void RequestMovePlayer(string direction)
    {
        var dir = Direction.tryParse(direction);

        if (dir == null)
            return;

        var intent = Intent.NewMovePlayerIntent(new MovePlayerIntent("Player", dir.Value));
        var request = Request.NewRequestIntent(intent);
        SessionManager.handleRequest("default", request);
    }

    protected override void BuildRenderTree(RenderTreeBuilder builder)
    {
    }
}