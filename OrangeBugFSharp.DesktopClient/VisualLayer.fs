namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Angle =
    | Deg of float32
    | Rad of float32
    member a.Radians = match a with Rad v -> v | Deg v -> v * pi / 180.0f
    member a.Degrees = match a with Deg v -> v | Rad v -> v * (180.0f / pi)

type VisualBrush =
    | NoBrush
    | SolidColorBrush of Color
    | Texture of Texture2D

type Visual = {
    offset: Vector2
    size: Vector2
    anchorPoint: Vector2 // normalized wrt. size of visual
    center: Vector2 // normalized wrt. size of visual, we rotate & scale around this point
    rotation: Angle
    scale: Vector2
    brush: VisualBrush
    clipToBounds: bool
}

module Visual =
    let empty = {
        offset = Vector2.Zero
        size = Vector2.Zero
        anchorPoint = Vector2.Zero
        center = Vector2.Zero
        rotation = Rad 0.0f
        scale = Vector2.One
        brush = NoBrush
        clipToBounds = true
    }

module VisualLayer =
    let flipY viewportHeight =
        Matrix.CreateScale(1.0f, -1.0f, 1.0f) *
        Matrix.CreateTranslation(0.0f, viewportHeight, 0.0f) * Matrix.Identity

    let computeLocalMatrix visual =
        let lerpToWidth, lerpToHeight = lerp 0.0f visual.size.X, lerp 0.0f visual.size.Y
        let lerpToScaledWidth, lerpToScaledHeight = lerp 0.0f (visual.size.X * visual.scale.X), lerp 0.0f (visual.size.Y * visual.scale.Y)
        Matrix.CreateTranslation(-(lerpToWidth visual.center.X), -(lerpToHeight visual.center.Y), 0.0f) *
        Matrix.CreateScale(visual.scale.X, visual.scale.Y, 1.0f) *
        Matrix.CreateRotationZ(visual.rotation.Radians) *
        Matrix.CreateTranslation(
            visual.offset.X + (lerpToScaledWidth <| visual.center.X - visual.anchorPoint.X),
            visual.offset.Y + (lerpToScaledHeight <| visual.center.Y - visual.anchorPoint.Y),
            0.0f
        )

    let quadVertices (size: Vector2) = 
        [|
            // To account for the flipped Y axis (y=0 is top), the UVs are flipped here (textures would be upside-down otherwise)
            VertexPositionNormalTexture(Vector3.Zero, Vector3.Backward, Vector2.Zero) // bottom left
            VertexPositionNormalTexture(Vector3.UnitY * size.Y, Vector3.Backward, Vector2.UnitY) // top left
            VertexPositionNormalTexture(Vector3.UnitX * size.X, Vector3.Backward, Vector2.UnitX) // bottom right
            VertexPositionNormalTexture(Vector3.One * Vector3(size.X, size.Y, 0.0f), Vector3.Backward, Vector2.One) // top right
        |]

    type State = {
        graphicsDevice: GraphicsDevice
        matrixStack: Matrix list
        effect: BasicEffect
    }

    let beginFrame (graphicsDevice: GraphicsDevice) =
        let w, h = float32 graphicsDevice.Viewport.Width, float32 graphicsDevice.Viewport.Height
        let viewMatrix = Matrix.Invert (Matrix.CreateTranslation(w / 2.0f, h / 2.0f, 0.0f))
        let projectionMatrix = Matrix.CreateOrthographic(w, h, -1000.0f, 1000.0f);

        let effect = new BasicEffect(graphicsDevice, View = viewMatrix, Projection = projectionMatrix)
        graphicsDevice.BlendState <- BlendState.AlphaBlend
        graphicsDevice.RasterizerState <- new RasterizerState(ScissorTestEnable = true, CullMode = CullMode.CullClockwiseFace)

        {
            graphicsDevice = graphicsDevice
            matrixStack = []
            effect = effect 
        }

    let endFrame state =
        match state.matrixStack with
        | [] -> state.effect.Dispose()
        | _ -> failwithf "Unbalanced push/pop-calls. Missing %i pop call(s)." state.matrixStack.Length

    let pushMatrix m state =
        { state with matrixStack = m :: state.matrixStack }

    let pushVisual visual state =
        let effect = state.effect
        let parentWorldMatrix =
            match state.matrixStack with
            | [] -> Matrix.Identity
            | m :: _ -> m
        let worldMatrix = computeLocalMatrix visual * parentWorldMatrix
        let viewportHeight = float32 state.graphicsDevice.Viewport.Height
        effect.World <- worldMatrix * (flipY viewportHeight)

        // Apply clipping
        state.graphicsDevice.ScissorRectangle <-
            if visual.clipToBounds
            then new Rectangle(0, 0, 80, 2000)
            else state.graphicsDevice.Viewport.Bounds

        // Apply brush
        match visual.brush with
        | NoBrush -> ()
        | SolidColorBrush color -> effect.DiffuseColor <- color.ToVector3()
        | Texture tex ->
            effect.Texture <- tex
            effect.TextureEnabled <- true
            effect.DiffuseColor <- Color.White.ToVector3()

        // Draw quad
        for pass in effect.CurrentTechnique.Passes do
            pass.Apply()
            state.graphicsDevice.DrawUserIndexedPrimitives(
                PrimitiveType.TriangleList,
                quadVertices visual.size, 0, 4,
                SpriteBatch3D.quadIndices, 0, 2
            )
        
        // Undo brush changes
        match visual.brush with
        | NoBrush -> ()
        | SolidColorBrush _ -> ()
        | Texture _ -> effect.TextureEnabled <- false

        { state with matrixStack = worldMatrix :: state.matrixStack }

    let pop state =
        match state.matrixStack with
        | [] -> failwith "No more open visuals to pop"
        | _ :: t -> { state with matrixStack = t }
