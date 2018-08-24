namespace OrangeBug

open LoxLib.Serialization
open OrangeBug.Game
open Newtonsoft.Json

module Serialization =
    // Serializes points as strings "x,y"
    type PointConverter() =
        inherit JsonConverter<Point>()

        override __.WriteJson(writer: JsonWriter, value: Point, _: JsonSerializer) =
            writer.WriteValue(sprintf "%i,%i" value.x value.y)

        override __.ReadJson(reader, _, _, _, _) =
            let s = reader.ReadAsString()
            match s.Split(',') with
            | [| sx; sy |] -> { x = int sx; y = int sy }
            | _ -> failwithf "Could not read a Point from string '%s'" s
    
    type SimTimeConverter() =
        inherit JsonConverter<SimTime>()

        override __.WriteJson(writer: JsonWriter, time: SimTime, _: JsonSerializer) =
            writer.WriteValue(time.value)

        override __.ReadJson(reader, _, _, _, _) =
            match reader.ReadAsInt32() |> Option.ofNullable with
            | Some time -> SimTime time
            | None -> failwithf "Could not read SimTime"

    type SimTimeSpanConverter() =
        inherit JsonConverter<SimTimeSpan>()

        override __.WriteJson(writer: JsonWriter, span: SimTimeSpan, _: JsonSerializer) =
            writer.WriteValue(span.value)

        override __.ReadJson(reader, _, _, _, _) =
            match reader.ReadAsInt32() |> Option.ofNullable with
            | Some span -> SimTimeSpan span
            | None -> failwithf "Could not read SimTimeSpan"
            
    type GameMapState with
        member this.toJson = 
            JsonConvert.SerializeObject(this,
                MapConverter(),
                SimTimeConverter(),
                SimTimeSpanConverter(),
                //PointConverter(),
                DiscriminatedUnionConverter())
