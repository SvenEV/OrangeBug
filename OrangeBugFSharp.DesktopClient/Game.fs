namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open OrangeBug.Game
open OrangeBug.DesktopClient.TileRendererNode

type Game() as g =
    inherit Microsoft.Xna.Framework.Game()

    do g.Content.RootDirectory <- "../../Assets" // TODO: This doesn't work
    do g.IsMouseVisible <- true
    let scene = Scene.create()
    let graphics = new GraphicsDeviceManager(g)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    override g.Initialize() =
        scene.Add "test" Scene.rootId { tile = WallTile }
        base.Initialize()

    override g.LoadContent() =
        spriteBatch <- new SpriteBatch(g.GraphicsDevice)

    override g.Update gameTime =
        if Keyboard.GetState().IsKeyDown(Keys.Escape) then g.Exit()
        TransformNode.update scene
        base.Update(gameTime)

    override g.Draw gameTime =
        g.GraphicsDevice.Clear Color.CornflowerBlue
        spriteBatch.Begin()
        TileRendererNode.draw scene (g.Content.Load<Texture2D>) spriteBatch
        spriteBatch.End()
        base.Draw(gameTime)