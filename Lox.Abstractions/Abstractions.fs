namespace LoxLib

open Newtonsoft.Json
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

type LoxCommand = {
    label: string
    [<JsonIgnore>]
    action: unit -> unit
}

type LogElement =
    | LogMessage of text: string
    | LogMarkdown of text: string
    | LogObject of label: string * o: obj
    | LogCommandBar of commands: LoxCommand list
    | LogImage of image: Image<Rgba32>

type Logger = {
    log: LogElement list -> unit
    setCommands: LoxCommand list -> unit
}

module Lox =
    let mutable logger = None

    let private action f =
        match logger with
        | None -> ()
        | Some logger -> f logger

    let log elements = action (fun logger -> logger.log elements)
    let print text = log [ LogMessage text ]
    let printmd text = log [ LogMarkdown text ]
    let setCommands commands = action (fun logger -> logger.setCommands commands)