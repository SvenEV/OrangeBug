module OrangeBug.DesktopClient.Program

[<EntryPoint>]
let main _ =
    LoxServer.init()
    use g = new Game()
    g.Run()
    0