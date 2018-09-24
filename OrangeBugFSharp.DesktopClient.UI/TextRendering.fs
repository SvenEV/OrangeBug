namespace OrangeBug.DesktopClient

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.Fonts
open SixLabors.Primitives
open Microsoft.Xna.Framework.Graphics

type TextWrapping =
    | NoWrap
    | Wrap
    | WrapWholeWords

type TextAlignment =
    | Left
    | Right
    | Center

type TextOptions = {
    fontFamily: string
    fontSize: float32
    fontStyle: FontStyle
    textWrapping: TextWrapping
    textAlignment: TextAlignment
    constraintSize: XnaVector2
}

module TextRendering =
    let measure (s: string) options =
        let font = SystemFonts.CreateFont(options.fontFamily, options.fontSize, options.fontStyle)
        let config =
            RendererOptions(font,
                ApplyKerning = false,
                WrappingWidth = (match options.textWrapping with NoWrap -> 0.0f | _ -> options.constraintSize.X),
                HorizontalAlignment =
                    match options.textAlignment with
                    | Left -> HorizontalAlignment.Left
                    | Right -> HorizontalAlignment.Right
                    | Center -> HorizontalAlignment.Center
            )
        
        let size = TextMeasurer.Measure(s, config)
        XnaVector2(size.Width, size.Height)

    let renderToTexture (s: string) options graphicsDevice =
        let font = SystemFonts.CreateFont(options.fontFamily, options.fontSize, options.fontStyle)
        let config =
            TextGraphicsOptions(
                WrapTextWidth = (match options.textWrapping with NoWrap -> 0.0f | _ -> options.constraintSize.X),
                HorizontalAlignment =
                    match options.textAlignment with
                    | Left -> HorizontalAlignment.Left
                    | Right -> HorizontalAlignment.Right
                    | Center -> HorizontalAlignment.Center
            )

        // TODO: The "+2" hack won't work if constraintSize.X < 'width required for text'
        // (hitting this issue here: https://github.com/SixLabors/ImageSharp/issues/688)
        let img = new Image<Rgba32>(int <| ceil options.constraintSize.X + 2.0f, int <| ceil options.constraintSize.Y)
        let brush = Brushes.Solid Rgba32.Black
        img.Mutate (fun ctx -> ctx.DrawText(config, s, font, brush, PointF.Empty) |> ignore)
        use stream = new MemoryStream()
        // TODO: For performance, don't do PNG-encoding - better use Texture2D.SetData(...)
        img.SaveAsPng stream
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        Texture2D.FromStream(graphicsDevice, stream)