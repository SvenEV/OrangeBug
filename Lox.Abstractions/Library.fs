module Lox

type LoxCommand = {
    label: string
    action: unit -> unit
}

type Logger = {
    print: string -> unit
    printmd: string -> unit
    setCommands: LoxCommand list -> unit
}

let mutable logger = None

let print text =
    match logger with
    | None -> ()
    | Some logger -> logger.print text

let printmd text =
    match logger with
    | None -> ()
    | Some logger -> logger.printmd text

let setCommands commands =
    match logger with
    | None -> ()
    | Some logger -> logger.setCommands commands