namespace LoxLib

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open System

type LoxCommand = {
    label: string
    action: unit -> unit
}

type LogElement =
    | LogString of text: string
    | LogMarkdown of text: string
    | LogObject of o: obj
    | LogCommandBar of commands: LoxCommand list
    | LogImage of image: Image<Rgba32>

type LogMessage = {
    elements: LogElement list
    category: string
    time: DateTimeOffset
}

type Logger = {
    log: LogMessage -> unit
    setCommands: LoxCommand list -> unit
}

module Lox =
    let mutable logger = None

    let private action f =
        match logger with
        | None -> ()
        | Some logger -> f logger

    let log category elements = action (fun logger ->
        logger.log {
            elements = elements
            category = category
            time = DateTimeOffset.Now
        })
    
    let print category text = log category [LogString text]
    let printmd category text = log category [LogMarkdown text]
    let setCommands commands = action (fun logger -> logger.setCommands commands)