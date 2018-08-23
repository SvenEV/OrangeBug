module Lox

type Logger = {
    print: string -> unit
    printmd: string -> unit
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