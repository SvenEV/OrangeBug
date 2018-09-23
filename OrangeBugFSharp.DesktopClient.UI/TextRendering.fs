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

module TextRendering =
    let measure (s: string) =
        let font = SystemFonts.CreateFont("Arial", 12.0f, FontStyle.Regular)
        let options = RendererOptions(font)
        let size = TextMeasurer.Measure(s, options)
        XnaVector2(size.Width, size.Height)

    let renderToTexture (s: string) (size: XnaVector2) graphicsDevice =
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