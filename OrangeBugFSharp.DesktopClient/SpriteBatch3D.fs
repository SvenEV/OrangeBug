namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework.Graphics

type Sprite = {
    texture: Texture2D
    anchorPoint: XnaVector2
    size: XnaVector2
}

type DrawSpriteCommand =
    | DrawSprite of Sprite * XnaMatrix

module SpriteBatch3D =

    let quadVertices = [|
        VertexPositionNormalTexture(XnaVector3.Zero, XnaVector3.Backward, XnaVector2.UnitY) // bottom left
        VertexPositionNormalTexture(XnaVector3.UnitY, XnaVector3.Backward, XnaVector2.Zero) // top left
        VertexPositionNormalTexture(XnaVector3.UnitX, XnaVector3.Backward, XnaVector2.One) // bottom right
        VertexPositionNormalTexture(XnaVector3.One, XnaVector3.Backward, XnaVector2.UnitX) // top right
    |]

    let draw graphicsDevice viewMatrix projectionMatrix commands  =
        use effect = new BasicEffect(graphicsDevice, View = viewMatrix, Projection = projectionMatrix, TextureEnabled = true)
        graphicsDevice.BlendState <- BlendState.AlphaBlend
        graphicsDevice.RasterizerState <- RasterizerState.CullNone

        let drawSprite sprite worldMatrix =
            let normalizedAnchor = XnaVector2(sprite.anchorPoint.X / float32 sprite.texture.Width, 1.0f - sprite.anchorPoint.Y / float32 sprite.texture.Height)
            let localMatrix =
                XnaMatrix.CreateScale(sprite.size.X, sprite.size.Y, 0.0f) *
                XnaMatrix.CreateTranslation(
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