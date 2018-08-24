module OrangeBug.DesktopClient.Program

[<EntryPoint>]
let main _ =
    LoxServer.init()
    Lox.setCommands [
        { label = "Quit"; action = fun () -> System.Environment.Exit(0) }
    ]
    use g = new Game()
    g.Run()
    0