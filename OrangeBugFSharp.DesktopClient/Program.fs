module OrangeBug.DesktopClient.Program

open LoxLib

[<EntryPoint>]
let main _ =
    LoxServer.init()
    use g = new Game()
    g.Run()
    0