namespace OrangeBug

module CharBitmap =
    open System.Text

    let sizeOf bitmap = Point.create (Array2D.length2 bitmap) (Array2D.length1 bitmap)

    let create size char = Array2D.init size.y size.x (fun _ _ -> char)

    let getPixel (bitmap: char[,]) p =
        bitmap.[p.y, p.x]

    let setPixel char (bitmap: char[,]) p =
        bitmap.[p.y, p.x] <- char

    let seqOfRect origin size =
        Seq.allPairs [0 .. size.x - 1] [0 .. size.y - 1]
        |> Seq.map (fun (x, y) -> Point.create x y)
        |> Seq.map ((+) origin)

    let seqOfRectBounds origin size =
        Seq.empty
        |> Seq.append ([origin.x .. origin.x + size.x - 1] |> Seq.map (fun x -> x, origin.y))
        |> Seq.append ([origin.x .. origin.x + size.x - 1] |> Seq.map (fun x -> x, origin.y + size.y - 1))
        |> Seq.append ([origin.y .. origin.y + size.y - 1] |> Seq.map (fun y -> origin.x, y))
        |> Seq.append ([origin.y .. origin.y + size.y - 1] |> Seq.map (fun y -> origin.x + size.x - 1, y))
        |> Seq.map (fun (x, y) -> Point.create x y)

    let seqOfPixels bitmap =
        seqOfRect Point.zero (sizeOf bitmap)
        |> Seq.map (fun p -> p, getPixel bitmap p)

    let toString (bitmap: char[,]) =
        let sb = StringBuilder()
        for y in [0 .. (sizeOf bitmap).y - 1] do
            for x in [0 .. (sizeOf bitmap).x - 1] do
                sb.Append (getPixel bitmap (Point.create x y)) |> ignore
            sb.AppendLine() |> ignore
        sb.ToString()

    let drawRectangle char origin size bitmap =
        seqOfRectBounds origin size |> Seq.iter (setPixel char bitmap)

    let drawBox origin size bitmap =
        seqOfRectBounds Point.zero size |> Seq.iter (fun p ->
            let char =
                match p.x, p.y with
                | 0, 0 -> '┌'
                | x, 0 when x = size.x - 1 -> '┐'
                | 0, y when y = size.y - 1 -> '└'
                | x, y when x = size.x - 1 && y = size.y - 1 -> '┘'
                | x, 0 -> '─'
                | x, y when y = size.y - 1 -> '─'
                | _ -> '│'
            setPixel char bitmap (origin + p))

    let fillRectangle char origin size (bitmap: char[,]) =
        seqOfRect origin size |> Seq.iter (setPixel char bitmap)

    let clear char bitmap =
        fillRectangle char Point.zero (sizeOf bitmap) bitmap

    let drawImage image bitmap origin =
        seqOfPixels image
        |> Seq.iter (fun (p, char) ->
            setPixel char bitmap (origin + p))