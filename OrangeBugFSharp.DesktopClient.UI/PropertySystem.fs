namespace OrangeBug.DesktopClient.LibUI

open System
open System.Collections
open System.Linq
open OrangeBug.DesktopClient
open System.Collections.Generic

type LayoutEffect =
    | NoLayoutEffect
    | AffectsMeasure
    | AffectsArrange

type IPropertyKey =
    abstract member Name: string
    abstract member PropertyType: Type
    abstract member DefaultValue: obj
    abstract member LayoutEffect: LayoutEffect

[<ReferenceEquality>]
type 'a PropertyKey =
    {
        name: string
        defaultValue: 'a
        layoutEffect: LayoutEffect
    }
    interface IPropertyKey with
        member key.Name = key.name
        member key.PropertyType = typedefof<'a>
        member key.DefaultValue = key.defaultValue :> obj
        member key.LayoutEffect = key.layoutEffect
    static member (@=) (key: 'a PropertyKey, value: 'a) =
        key :> IPropertyKey, value :> obj

[<RequireQualifiedAccess>]
module PropertyKey =
    let register name defaultValue layoutEffect = {
        name = name
        defaultValue = defaultValue
        layoutEffect = layoutEffect
    }

type PropertyBag(entries: seq<IPropertyKey * obj>) =
    static member Empty = PropertyBag(Enumerable.Empty())
    member val private props = Dictionary.ofTupleSeq entries
    member bag.get (key: 'a PropertyKey) =
        match bag.props.TryFind key with
        | Some value -> value :?> 'a
        | None -> key.defaultValue
    member bag.get (key: IPropertyKey) =
        match bag.props.TryFind key with
        | Some value -> value
        | None -> key.DefaultValue
    interface IEquatable<PropertyBag> with
        member a.Equals(b) = Dictionary.equal a.props b.props
    interface IEnumerable<IPropertyKey * obj> with
        member bag.GetEnumerator(): IEnumerator =
            (bag.props |> Seq.map KeyValuePair.asTuple).GetEnumerator() :> IEnumerator
        member bag.GetEnumerator(): IEnumerator<IPropertyKey * obj> =
            (bag.props |> Seq.map KeyValuePair.asTuple).GetEnumerator()
    override a.Equals(b) =
        match b with
        | :? PropertyBag as b -> Dictionary.equal a.props b.props
        | _ -> false
    override bag.GetHashCode() =
        bag.props.GetHashCode()

[<AutoOpen>]
module PropertySystem =
    let get (props: PropertyBag) key = props.get key

    let diffProps (oldProps: PropertyBag) (newProps: PropertyBag) =
        (oldProps, newProps)
        ||> Seq.fullOuterJoin fst fst (fun a b key -> if a = b then None else Some key)
        |> Seq.choose id