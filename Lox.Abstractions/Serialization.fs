module LoxLib.Serialization

open System
open System.Collections
open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq

type MapConverter() =
    inherit JsonConverter()

    override __.WriteJson(writer, value, serializer) =
        let entries =
            let enumerator = (value :?> IEnumerable).GetEnumerator()
            seq {
                while enumerator.MoveNext() do
                    let kvp = enumerator.Current
                    let key = kvp.GetType().GetProperty("Key").GetValue(kvp)
                    let value = kvp.GetType().GetProperty("Value").GetValue(kvp)
                    yield key, value
            }
            |> Seq.map (fun (key, value) -> (JToken.FromObject(key, serializer), value))
            |> Seq.toArray

        let stringKeysOnly = entries |> Seq.forall (fun (key, _) -> key.Type = JTokenType.String)

        if stringKeysOnly then
            // serialize map as object: { "<key>": <value>, ... }
            writer.WriteStartObject()
            entries |> Seq.iter (fun (key, value) ->
                writer.WritePropertyName(string (key :?> JValue).Value)
                serializer.Serialize(writer, value))
            writer.WriteEndObject()
        else
            // serialize map as array: [ { "key": <key>, "value": <value> }, ... ]
            writer.WriteStartArray()
            entries |> Seq.iter (fun (key, value) ->
                writer.WriteStartObject()
                writer.WritePropertyName("key")
                serializer.Serialize(writer, key)
                writer.WritePropertyName("value")
                serializer.Serialize(writer, value)
                writer.WriteEndObject())
            writer.WriteEndArray()

    // TODO: Implement reading array or object
    override __.ReadJson(reader, destinationType, existingValue, serializer) =
        reader.Read() |> ignore // skip StartArray

        let mutable map = Activator.CreateInstance(destinationType)
        let addMethod = destinationType.GetMethod("Add")
        let addEntry key value =
            map <- addMethod.Invoke(map, [| key, value |])

        while reader.TokenType <> JsonToken.EndArray do
            reader.Read() |> ignore // skip StartObject
            reader.Read() |> ignore // skip PropertyName 'key'
            let key = serializer.Deserialize(reader)
            reader.Read() |> ignore // skip PropertyName 'value'
            let value = serializer.Deserialize(reader)
            addEntry key value
        
        map

    override __.CanConvert(objectType) =
        objectType.IsConstructedGenericType &&
        objectType.GetGenericTypeDefinition() = typedefof<Map<_, _>>

// Adapted from http://www.hoonzis.com/fsharp-json-serializaton/
// - Serializes cases as string if the union type is "enum-like" (i.e. none of the cases has fields)
// - Serializes cases as { "$type": "", "field1": ..., ... } otherwise
type DiscriminatedUnionConverter() =
    inherit JsonConverter()

    let isEnum unionType =
        FSharpType.GetUnionCases unionType
        |> Seq.forall (fun case -> case.GetFields().Length = 0)

    override __.WriteJson(writer, value, serializer) =
        let unionType = value.GetType()
        let case, fieldValues = FSharpValue.GetUnionFields(value, unionType)
        let fields = case.GetFields()

        match isEnum unionType with
        | true -> writer.WriteValue(case.Name)
        | false ->
            writer.WriteStartObject()
            writer.WritePropertyName "$type"
            writer.WriteValue(case.Name)
            for i in 0 .. fieldValues.Length - 1 do
                writer.WritePropertyName fields.[i].Name
                serializer.Serialize(writer, fieldValues.[i])
            writer.WriteEndObject()

    override __.ReadJson(reader, destinationType, existingValue, serializer) =
        let fields =
            if reader.TokenType <> JsonToken.StartObject then
                [| reader.Value |]
            else
                reader
                |> Seq.unfold (fun reader ->
                    if reader.Read()
                    then Some((reader.TokenType, reader.Value), reader)
                    else None)
                |> Seq.takeWhile(fun (token, _) -> token <> JsonToken.EndObject)
                |> Seq.pairwise
                |> Seq.mapi (fun id value -> id, value)
                |> Seq.filter (fun (id, _) -> id % 2 = 0)
                |> Seq.map snd
                |> Seq.map (fun (_, (_, v)) -> v)
                |> Seq.toArray

        let unionCases = FSharpType.GetUnionCases(destinationType)
        let valuesOnly = fields |> Seq.skip 1 |> Array.ofSeq
        let case = unionCases |> Seq.tryFind (fun uc -> uc.Name = string fields.[0])

        match case, isEnum destinationType with
            | Some case, true -> FSharpValue.MakeUnion(case, [||])
            | Some case, false -> FSharpValue.MakeUnion(case, valuesOnly)
            | None, _ -> failwithf "Could not find case '%s' for destination type '%s'" (string fields.[0]) (string destinationType)

    override __.CanConvert(objectType) =
        FSharpType.IsUnion objectType &&
        //it seems that both option and list are implemented using discriminated unions, so we tell json.net to ignore them and use different serializer
        not (objectType.IsGenericType  && objectType.GetGenericTypeDefinition() = typedefof<list<_>>) &&
        not (objectType.IsGenericType  && objectType.GetGenericTypeDefinition() = typedefof<option<_>>) &&
        not (FSharpType.IsRecord objectType)