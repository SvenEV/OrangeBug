namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open OrangeBug
open OrangeBug.Game
open OrangeBug.DesktopClient.TransformNode
open OrangeBug.DesktopClient.TileRendererNode
open OrangeBug.Hosting
open Microsoft.Xna.Framework.Graphics
open System
open EntityRendererNode
open Microsoft.Xna.Framework.Input

type Game() as g =
    inherit Microsoft.Xna.Framework.Game()

    do g.Content.RootDirectory <- @"D:\Projects\Experimental\OrangeBugFSharp\OrangeBugFSharp.DesktopClient\bin\DesktopGL\Assets" //"../../Assets" // TODO: This doesn't work
    do g.IsMouseVisible <- true
    do g.Window.AllowUserResizing <- true
    let graphics = new GraphicsDeviceManager(g)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable scene = EmptyTree

    let handleEvent ev =
        match ev.event with
        | EntityMovedEvent ev ->
            let nodeId = sprintf "Entity(%i)" ev.entityId.id
            scene |> SceneGraph.update nodeId (fun state -> { (state :?> EntityRendererNodeState) with position = ev.newPosition })
        |_ -> ()

    let onSignal =
        function
        | ReceiveInitialMap (map, time, tickTargetTime) ->
            let tiles =
                map.tiles
                |> Grid.asSeq
                |> Seq.map (fun (p, t) -> TileRendererNode.createSubtree p t.tile)

            let entities =
                map.entities
                |> Seq.map (fun kvp -> EntityRendererNode.createSubtree kvp.Key kvp.Value.position kvp.Value.entity)

            scene <- TreeNode (SceneGraph.rootNode, Seq.append tiles entities |> List.ofSeq)
        | ReceiveEvents (events, time) ->
            events |> Seq.iter handleEvent
        | _ -> ()


    override g.Initialize() =
        SessionManager.create onSignal (Simulation.create <| SampleMaps.createInitialMap()) "game"
        base.Initialize()

    override g.LoadContent() =
        spriteBatch <- new SpriteBatch(g.GraphicsDevice)

    override g.Update gameTime =
        if Keyboard.GetState().IsKeyDown(Keys.Escape) then g.Exit()

        let requestMove dir = SessionManager.handleRequest "game" (RequestIntent (MovePlayerIntent { name = "Player"; direction = dir }))
        if Keyboard.GetState().IsKeyDown(Keys.Right) then requestMove East
        if Keyboard.GetState().IsKeyDown(Keys.Left) then requestMove West
        if Keyboard.GetState().IsKeyDown(Keys.Up) then requestMove North
        if Keyboard.GetState().IsKeyDown(Keys.Down) then requestMove South
        
        TransformNode.update scene
        TileRendererNode.update scene (g.Content.Load<Texture2D>)
        EntityRendererNode.update scene (g.Content.Load<Texture2D>)
        base.Update(gameTime)

    override g.Draw gameTime =
        g.GraphicsDevice.Clear Color.CornflowerBlue
        
        spriteBatch.Begin(transformMatrix = Nullable(Matrix.CreateScale(50.0f)))
        SpriteRendererNode.draw scene spriteBatch
        spriteBatch.End()
        base.Draw(gameTime)