namespace OrangeBug.DesktopClient

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Quad = {
    vertices: VertexPositionColorTexture[]
}

module Quad =
    let indices = [| 0; 1; 2; 2; 1; 3 |]

    let to3d (v: Vector2) = Vector3(v.X, v.Y, 0.0f)

    let create (position: Vector2) (size: Vector2) color = {
        vertices = [|
            // To account for the flipped Y axis (y=0 is top), the UVs are flipped here (textures would be upside-down otherwise)
            VertexPositionColorTexture(to3d position, color, Vector2.Zero) // bottom left
            VertexPositionColorTexture(to3d (position + Vector2.UnitY * size.Y), color, Vector2.UnitY) // top left
            VertexPositionColorTexture(to3d (position + Vector2.UnitX * size.X), color, Vector2.UnitX) // bottom right
            VertexPositionColorTexture(to3d (position + size), color, Vector2.One) // top right
        |]
    }

    let createUV (position: Vector2) (size: Vector2) color (uvMin: Vector2) (uvMax: Vector2) = {
        vertices = [|
            // To account for the flipped Y axis (y=0 is top), the UVs are flipped here (textures would be upside-down otherwise)
            VertexPositionColorTexture(to3d position, color, uvMin) // bottom left
            VertexPositionColorTexture(to3d (position + Vector2.UnitY * size.Y), color, Vector2(uvMin.X, uvMax.Y)) // top left
            VertexPositionColorTexture(to3d (position + Vector2.UnitX * size.X), color, Vector2(uvMax.X, uvMin.Y)) // bottom right
            VertexPositionColorTexture(to3d (position + size), color, uvMax) // top right
        |]
    }

type TextWrapping =
    | NoWrap
    | Wrap
    | WrapWholeWords

// Approach 1: Like SpriteBatch, render each character as a textured quad
module TextRendering =

    type Rectangle with
        member r.TopLeft = vec2 (float32 r.X) (float32 r.Y)
        member r.BottomRight = vec2 (float32 r.Right) (float32 r.Bottom)
        member r.SizeF = vec2 (float32 r.Width) (float32 r.Height)

    type Texture2D with
        member t.Size = vec2 (float32 t.Width) (float32 t.Height)

    let render (s: string) (font: SpriteFont) =
        let lineBreak (offset: Vector2) = Vector2(0.0f, offset.Y + (float32 font.LineSpacing))
        let glyphs = font.GetGlyphs()

        let defaultGlyph =
            if font.DefaultCharacter.HasValue
            then Some glyphs.[font.DefaultCharacter.Value]
            else None

        let (|KnownGlyph|_|) c =
            match glyphs.TryGetValue c with
            | true, g -> Some g
            | _ -> defaultGlyph

        let handleChar offset = function
            | '\r' -> None, offset
            | '\n' -> None, lineBreak offset
            | KnownGlyph glyph -> 
                let nextOffset =
                    if offset.X = 0.0f // first glyph of line?
                    then Vector2(max 0.0f glyph.LeftSideBearing, offset.Y)
                    else Vector2(offset.X + font.Spacing + glyph.LeftSideBearing, offset.Y)
                let bounds = glyph.BoundsInTexture
                let position = nextOffset + glyph.Cropping.TopLeft
                let uvMin = bounds.TopLeft / font.Texture.Size
                let uvMax = bounds.BottomRight / font.Texture.Size
                let quad = Quad.createUV position (bounds.SizeF) Color.Black uvMin uvMax
                Some quad, nextOffset + Vector2(glyph.Width + glyph.RightSideBearing, 0.0f)
            | c -> failwithf "Could not find glyph for character '%A'" c

        let quads, _finalOffset = s |> Seq.mapFold handleChar Vector2.Zero
        let quads = quads |> Seq.choose id
        quads

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.Fonts
open SixLabors.Primitives
open System.IO

// Approach 2: Render whole text on a texture (using ImageSharp)
module TextRendering2 =

    let measure (s: string) =
        let font = SystemFonts.CreateFont("Arial", 12.0f, FontStyle.Regular)
        let options = RendererOptions(font)
        let size = TextMeasurer.Measure(s, options)
        vec2 size.Width size.Height

    let renderToTexture (s: string) (size: Vector2) graphicsDevice =
        let font = SystemFonts.CreateFont("Arial", 12.0f, FontStyle.Regular)
        // let options = RendererOptions(font)
        let img = new Image<Rgba32>(int <| ceil size.X + 2.0f, int <| ceil size.Y)
        let brush = Brushes.Solid Rgba32.Black
        img.Mutate (fun ctx -> ctx.DrawText(s, font, brush, PointF.Empty) |> ignore)
        use stream = new MemoryStream()
        // TODO: For performance, don't do PNG-encoding - better use Texture2D.SetData(...)
        img.SaveAsPng stream
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        Texture2D.FromStream(graphicsDevice, stream)