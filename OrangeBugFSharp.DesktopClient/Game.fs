namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Game() as g =
    inherit Microsoft.Xna.Framework.Game()

    do g.Content.RootDirectory <- "Content"
    do g.IsMouseVisible <- true
    let graphics = new GraphicsDeviceManager(g)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    override g.Initialize() =
        base.Initialize()

    override g.LoadContent() =
        spriteBatch <- new SpriteBatch(g.GraphicsDevice)

    override g.Update gameTime =
        if Keyboard.GetState().IsKeyDown(Keys.Escape) then g.Exit()
        base.Update(gameTime)

    override g.Draw gameTime =
        g.GraphicsDevice.Clear Color.CornflowerBlue
        
        base.Draw(gameTime)