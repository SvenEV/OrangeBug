module LoxLib.Dto

open System
open System.IO
open CommonMark
open Newtonsoft.Json
open SixLabors.ImageSharp

type CommandDto = {
    label: string
}

type LogElementDto =
    | LogStringDto of text: string
    | LogMarkdownDto of html: string
    | LogObjectDto of label: string * json: string
    | LogCommandBarDto of commands: CommandDto list
    | LogImageDto of data: string

type LogMessageDto = {
    id: int
    elements: LogElementDto list
    category: string
    time: string
}

let mapCommand (command: LoxCommand) = { label = command.label }

let mapLogElement = function
    | LogString text -> LogStringDto text
    | LogMarkdown text -> LogMarkdownDto (CommonMarkConverter.Convert text)
    | LogObject o -> LogObjectDto (o.GetType().Name, (JsonConvert.SerializeObject o))
    | LogCommandBar commands -> LogCommandBarDto (commands |> List.map mapCommand)
    | LogImage image ->
        use stream = new MemoryStream()
        image.SaveAsPng stream
        let base64 = "data:image/png;base64," + Convert.ToBase64String(stream.ToArray())
        LogImageDto base64

let mapMessage (message: LogMessage) messageId = {
    id = messageId
    elements = message.elements |> List.map mapLogElement
    category = message.category
    time = message.time.ToString("yyyy-MM-dd hh:mm:ss")
}