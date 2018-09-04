namespace OrangeBug.DesktopClient

open LoxLib
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open OrangeBug

type Game() as g =
    inherit Microsoft.Xna.Framework.Game()

    do g.Content.RootDirectory <- @"D:\Projects\Experimental\OrangeBugFSharp\OrangeBugFSharp.DesktopClient\bin\DesktopGL\Assets" //"../../Assets" // TODO: This doesn't work
    do g.IsMouseVisible <- true
    do g.Window.AllowUserResizing <- true
    let graphics = new GraphicsDeviceManager(g, PreferMultiSampling = true, HardwareModeSwitch = true)
    let mutable session = None

    let getSprite name =
        let texture = g.Content.Load<Texture2D> name
        { texture = texture; size = Vector2.One; anchorPoint = 100.0f * Vector2.One }

    override g.Initialize() =
        session <- Some (GameSession.create "mainSession")
        Lox.setCommands [
            { label = "Quit"; action = fun () -> System.Environment.Exit(0) }
            {
                label = "Snapshot"
                action = fun () ->
                    Lox.log "Snapshot" [
                        LogMarkdown ("**Simulation at " + session.Value.simulation.time.value.ToString() + "**")
                        LogImage (Rendering.mapAsImage session.Value.simulation.map)
                        LogObject session.Value.simulation
                    ]
            }
        ]
        base.Initialize()

    override g.LoadContent() = ()

    override g.Update gameTime =
        base.Update gameTime
        if Keyboard.GetState().IsKeyDown Keys.Escape then g.Exit()
        match session with
        | None -> ()
        | Some session ->
            let aspectRatio = g.GraphicsDevice.Viewport.AspectRatio
            GameSession.update session getSprite aspectRatio gameTime
            g.Window.Title <- sprintf "Time: %i, FPS: %i, slow: %b" session.simulation.time.value (int <| 1.0 / gameTime.ElapsedGameTime.TotalSeconds) gameTime.IsRunningSlowly
        
    override g.Draw gameTime =
        base.Draw gameTime
        g.GraphicsDevice.Clear Color.Black
        match session with
        | None -> ()
        | Some session -> GameSession.draw session g.GraphicsDevice