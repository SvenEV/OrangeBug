namespace OrangeBug.DesktopClient.LibUI

open System
open System.Collections.Generic
open Layman
open Layman.Layouts
open OrangeBug.DesktopClient
open Microsoft.Xna.Framework

[<AutoOpen>]
module PropertySystem =
    [<ReferenceEquality>]
    type PropertyKey =
        {
            name: string
            propType: Type
            defaultValue: obj
        }
        static member (@=) (key: PropertyKey, value: 'a) = key, value :> obj // TODO: dynamic type check
        interface IComparable with
            member x.CompareTo yObj =
                match yObj with
                | :? PropertyKey as y -> compare x.name y.name
                | _ -> invalidArg "yObj" "Cannot compare values of different types"

    let declareProperty<'a> name (defaultValue: 'a) = {
        name = name
        propType = typedefof<'a>
        defaultValue = defaultValue
    }

    type PropertyBag = Map<PropertyKey, obj>
    let toPropertyBag = Map.ofList

    let get<'a> (props: PropertyBag) key =
        match props.TryFind key with
        | Some value -> value :?> 'a
        | None -> key.defaultValue :?> 'a

type IElementState = interface end

[<ReferenceEquality>]
type ElementBehavior = {
    name: string
    mount: PropertyBag -> Map<PropertyKey, ElementInstance list> -> IElementState // computes the initial state when a new instance of this element is created
    unmount: PropertyBag -> IElementState -> unit // performs any cleanup tasks when the element is destroyed
    render: PropertyBag -> IElementState -> VisualLayer.State -> VisualLayer.State // is called at 60 fps to draw the UI
    layout: PropertyBag -> IElementState -> LayoutPhase -> LayoutResult
}

and ElementInstance = {
    behavior: ElementBehavior
    mutable props: PropertyBag
    mutable children: Map<PropertyKey, ElementInstance list>
    mutable state: IElementState
}

// this is only used to construct our "virtual DOM"
type ElementNode = {
    behavior: ElementBehavior
    props: PropertyBag
}

//
//  Some Typical Components
//

[<AutoOpen>]
module UIElement =
    type Vector2 = Microsoft.Xna.Framework.Vector2 // to avoid clashes with Layman's Vector2
    let layvec2 = Layman.BasicConstructors.vec2

    let Key = declareProperty "Key" (None: IComparable option)
    let Width = declareProperty "Width" nan
    let Height = declareProperty "Height" nan
    let Margin = declareProperty "Margin" Thickness.zero
    let Padding = declareProperty "Padding" Thickness.zero
    let HorizontalAlignment = declareProperty "HorizontalAlignment" Alignment.Stretch
    let VerticalAlignment = declareProperty "VerticalAlignment" Alignment.Stretch

module Zero =
    let ZeroState = { new IElementState }

    let Component = {
        name = "Zero"
        mount = fun _ _ -> ZeroState
        unmount = fun _ _ -> ()
        render = fun _ _ -> id
        layout = fun _ _ -> Layman.Layouts.DefaultLayouts.zero
    }
    let zero = {
        behavior = Component
        props = Map.empty
    }

module Border =
    let Child = declareProperty "Child" Zero.zero
    let Background = declareProperty "Background" (SolidColorBrush Color.Magenta)

    type State =
        {
            layoutCache: LayoutCache
            child: ElementInstance option
        }
        interface IElementState

    let mount props (children: Map<PropertyKey, ElementInstance list>) =
        {
            layoutCache = LayoutCache.create()
            child = children |> Map.tryFind Child |> Option.map (fun c -> c.[0])
        } :> IElementState
    
    let unmount props state = ()

    let layout props (state: IElementState) =
        let state = state :?> State

        let childLayout =
            match state.child with
            | Some child -> child.behavior.layout child.props child.state
            | None -> zero

        let props =
            { StandardLayouts.defaultProps with
                size = layvec2 (get props Width) (get props Height)
                horizontalAlignment = get props HorizontalAlignment
                verticalAlignment = get props VerticalAlignment
                margin = get props Margin
                padding = get props Padding
            }
        StandardLayouts.standardLayout props state.layoutCache childLayout

    // At this point, all children are realized in the constructed UI tree.
    // No need to look through props to find children to render, just access component state.
    // This function must be fast, like 60 fps!
    let render props (state: IElementState) (ctx: VisualLayer.State) =
        let state = state :?> State
        let cache = state.layoutCache

        let childRender =
            match state.child with
            | Some child -> child.behavior.render child.props child.state
            | None -> id

        ctx
        |> VisualLayer.pushVisual
            {
                Visual.empty with
                    offset = Vector2(float32 cache.relativeBounds.X, float32 cache.relativeBounds.Y)
                    size = Vector2(float32 cache.relativeBounds.Width, float32 cache.relativeBounds.Height)
                    brush = get props Background
            }
        |> childRender
        |> VisualLayer.pop

    let Component = {
        name = "Border"
        mount = mount
        unmount = unmount
        render = render
        layout = layout
    }

    let border props = {
        behavior = Component
        props = toPropertyBag props
    }

module UI =
    let (|KeyValuePair|) (kvp: KeyValuePair<'key, 't>) = kvp.Key, kvp.Value

    let fullOuterJoin keyA keyB projection a b =
        let alookup = a |> Seq.map (fun v -> keyA v, v) |> dict // assumption: no duplicate keys
        let blookup = b |> Seq.map (fun v -> keyB v, v) |> dict
        Set.union (set alookup.Keys) (set blookup.Keys)
        |> Seq.map (fun key ->
            let xa = match alookup.TryGetValue key with true, v -> Some v | _ -> None
            let xb = match blookup.TryGetValue key with true, v -> Some v | _ -> None
            projection xa xb key)

    type Diff =
        | NoDiff of ElementInstance * ElementNode
        | Update of ElementInstance * ElementNode
        | Unmount of ElementInstance
        | Mount of ElementNode

    let diffChildren (instances: ElementInstance seq) descriptions =
        let elementKey i props = get props Key |> Option.defaultValue (i :> IComparable)
        let keyedDescriptions = descriptions |> Seq.mapi (fun i desc -> elementKey i desc.props, desc)
        let keyedInstances = instances |> Seq.mapi (fun i inst -> elementKey i inst.props, inst)

        // Now we need an outer join
        (keyedInstances, keyedDescriptions)
        ||> fullOuterJoin fst fst (fun inst desc _ ->
            match inst, desc with
            | Some(_, inst), Some(_, desc) ->
                if inst.props = desc.props
                then NoDiff(inst, desc)
                else Update(inst, desc)
            | Some(_, inst), None -> Unmount inst
            | None, Some(_, desc) -> Mount desc
            | None, None -> failwith "This should not happen")
    
    let diffChildrenAllProps (instances: Map<PropertyKey, ElementInstance list>) node =
        // Find props holding a single child element and recursively instantiate them
        let singleChildProps =
            node.props
            |> Seq.filter (fun (KeyValuePair(key, _)) -> key.propType = typedefof<ElementNode>)
            |> Seq.map (fun (KeyValuePair(key, node)) -> key, Seq.singleton (node :?> ElementNode))

        // Find props holding collections of child elements and recursively instantiate them
        let multiChildProps = 
            node.props
            |> Seq.filter (fun (KeyValuePair(key, _)) -> typedefof<ElementNode seq>.IsAssignableFrom key.propType)
            |> Seq.map (fun (KeyValuePair(key, nodes)) -> key, (nodes :?> ElementNode seq))
            |> Seq.filter (fun (key, nodes) -> not (Seq.isEmpty nodes))

        Seq.append singleChildProps multiChildProps
        |> Map.ofSeq
        |> Map.map (fun prop descriptions ->
            let instances = Map.tryFind prop instances |> Option.defaultValue []
            diffChildren instances descriptions)

    let rec updateChildrenAllProps instances node =
        let handleProp _ diffs =
                diffs
                |> Seq.map (function
                    | NoDiff(inst, _) -> Some inst
                    | Update(inst, desc) -> Some (updateSubtree inst desc)
                    | Mount desc -> Some (mountSubtree desc)
                    | Unmount inst -> unmountSubtree inst; None)
                |> Seq.filter (fun inst -> inst.IsSome)
                |> Seq.map (fun inst -> inst.Value)
                |> List.ofSeq

        diffChildrenAllProps instances node |> Map.map handleProp

    and unmountSubtree (tOld: ElementInstance) =
        tOld.behavior.unmount tOld.props tOld.state
        tOld.children |> Map.iter (fun _ -> Seq.iter unmountSubtree)

    and mountSubtree (tNew: ElementNode) =
        let allChildren = updateChildrenAllProps Map.empty tNew
        let state = tNew.behavior.mount tNew.props allChildren
        {
            behavior = tNew.behavior
            props = tNew.props
            children = allChildren
            state = state
        }

    and updateSubtree (tOld: ElementInstance) (tNew: ElementNode) =
        // compare old and new:
        // if behavior and props are identical, stop recursion (subtree doesn't change)
        // otherwise, update props and recurse
        if tNew.behavior = tOld.behavior && tNew.props = tOld.props then
            tOld
        else
            {
                behavior = tOld.behavior // tNew has same behavior
                props = tNew.props
                children = updateChildrenAllProps tOld.children tNew
                state = tOld.state
            }

    // After a UI tree (or a part of it) was (re-)rendered, use this function
    // to determine and apply changes to the actual element instances.
    let update (tOld: ElementInstance option) (tNew: ElementNode) =
        match tOld with
        | None -> mountSubtree tNew
        | Some tOld -> updateSubtree tOld tNew

module UISample =
    let sample() =
        Border.border [
            UIElement.HorizontalAlignment @= Layman.Alignment.End
            UIElement.Width @= 100.0
            UIElement.Height @= 100.0
            Border.Background @= SolidColorBrush Color.Red
            Border.Child @= Border.border [
                UIElement.Height @= 20.0
                UIElement.Margin @= Thickness.createUniform 4.0
                UIElement.VerticalAlignment @= Center
                Border.Background @= SolidColorBrush Color.DeepSkyBlue
            ]
        ]