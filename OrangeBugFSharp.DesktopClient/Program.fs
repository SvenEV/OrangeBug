module OrangeBugFSharp.DesktopClient.Program

[<EntryPoint>]
let main _ =
    use g = new Game()
    g.Run()
    0