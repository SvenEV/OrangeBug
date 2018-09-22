namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework

type Sprite = {
    texture: Texture2D
    anchorPoint: Vector2
    size: Vector2
}

type DrawSpriteCommand =
    | DrawSprite of Sprite * Matrix

module SpriteBatch3D =

    let quadVertices = [|
        VertexPositionNormalTexture(Vector3.Zero, Vector3.Backward, Vector2.UnitY) // bottom left
        VertexPositionNormalTexture(Vector3.UnitY, Vector3.Backward, Vector2.Zero) // top left
        VertexPositionNormalTexture(Vector3.UnitX, Vector3.Backward, Vector2.One) // bottom right
        VertexPositionNormalTexture(Vector3.One, Vector3.Backward, Vector2.UnitX) // top right
    |]

    let draw graphicsDevice viewMatrix projectionMatrix commands  =
        use effect = new BasicEffect(graphicsDevice, View = viewMatrix, Projection = projectionMatrix, TextureEnabled = true)
        graphicsDevice.BlendState <- BlendState.AlphaBlend
        graphicsDevice.RasterizerState <- RasterizerState.CullNone

        let drawSprite sprite worldMatrix =
            let normalizedAnchor = Vector2(sprite.anchorPoint.X / float32 sprite.texture.Width, 1.0f - sprite.anchorPoint.Y / float32 sprite.texture.Height)
            let localMatrix =
                Matrix.CreateScale(sprite.size.X, sprite.size.Y, 0.0f) *
                Matrix.CreateTranslation(
                    -(lerp 0.0f sprite.size.X normalizedAnchor.X),
                    -(lerp 0.0f sprite.size.Y normalizedAnchor.Y),
                    0.0f
                )

            effect.World <- localMatrix * worldMatrix
            effect.Texture <- sprite.texture

            for pass in effect.CurrentTechnique.Passes do
                pass.Apply()
                graphicsDevice.DrawUserIndexedPrimitives(
                    PrimitiveType.TriangleList,
                    quadVertices, 0, 4,
                    Quad.indices, 0, 2
                )
        
        commands |> Seq.iter (fun (DrawSprite (s, m)) -> drawSprite s m)