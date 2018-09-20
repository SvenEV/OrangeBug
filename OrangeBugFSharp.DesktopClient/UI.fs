namespace OrangeBug.DesktopClient.LibUI

open System
open System.Collections.Generic
open Layman
open Layman.Layouts
open OrangeBug.DesktopClient
open Microsoft.Xna.Framework
open System.Collections.Immutable

module ImmutableDictionary =
    let tryFind key (dict: ImmutableDictionary<'key, 't>) =
        match dict.TryGetValue key with
        | true, v -> Some v
        | _ -> None
    
    let ofSeq items =
        items
        |> Seq.map (fun (key, value) -> KeyValuePair(key, value))
        |> ImmutableDictionary.CreateRange

    let iter f (dict: ImmutableDictionary<'key, 't>) =
        dict :> IEnumerable<KeyValuePair<'key, 't>>
        |> Seq.iter (fun kvp -> f kvp.Key kvp.Value)

type IElementState = interface end

[<ReferenceEquality>]
type PropertyKey =
    {
        name: string
        propType: Type
        defaultValue: obj
    }
    static member (@=) (key: PropertyKey, value: 'a) = KeyValuePair(key, value :> obj) // TODO: dynamic type check

type PropertyBag = ImmutableDictionary<PropertyKey, obj>
type ElementInstanceBag = ImmutableDictionary<PropertyKey, ElementInstance list>

and ElementInfo = {
    behavior: ElementBehavior
    props: PropertyBag
    state: IElementState
    getParent: unit -> ElementInfo option
    getChildren: PropertyKey -> ElementInfo list
}

and [<ReferenceEquality>] ElementBehavior = {
    name: string
    mount: PropertyBag -> IElementState // computes the initial state when a new instance of this element is created
    unmount: ElementInfo -> unit // performs any cleanup tasks when the element is destroyed
    render: ElementInfo -> VisualLayer.State -> VisualLayer.State // is called at 60 fps to draw the UI
    layout: ElementInfo -> LayoutPhase -> LayoutResult
}

and ElementInstance = {
    behavior: ElementBehavior
    parent: ElementInstance option
    mutable props: PropertyBag
    mutable children: ElementInstanceBag
    mutable state: IElementState
}

[<AutoOpen>]
module PropertySystem =
    let declareProperty<'a> name (defaultValue: 'a) = {
        name = name
        propType = typedefof<'a>
        defaultValue = defaultValue
    }

    let toPropertyBag list = ImmutableDictionary.CreateRange(list)

    let get<'a> (props: PropertyBag) key =
        match props.TryGetValue key with
        | true, value -> value :?> 'a
        | _ -> key.defaultValue :?> 'a

// this is only used to construct our "virtual DOM"
type Element = {
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
    let MinWidth = declareProperty "MinWidth" 0.0
    let MinHeight = declareProperty "MinHeight" 0.0
    let MaxWidth = declareProperty "MaxWidth" infinity
    let MaxHeight = declareProperty "MaxHeight" infinity
    let Margin = declareProperty "Margin" Thickness.zero
    let Padding = declareProperty "Padding" Thickness.zero
    let HorizontalAlignment = declareProperty "HorizontalAlignment" Alignment.Stretch
    let VerticalAlignment = declareProperty "VerticalAlignment" Alignment.Stretch

    let standardLayout props (layoutCache: LayoutCache) childLayout =
        let props =
            { StandardLayouts.defaultProps with
                size = layvec2 (get props Width) (get props Height)
                minSize = layvec2 (get props MinWidth) (get props MinHeight)
                maxSize = layvec2 (get props MaxWidth) (get props MaxHeight)
                horizontalAlignment = get props HorizontalAlignment
                verticalAlignment = get props VerticalAlignment
                margin = get props Margin
                padding = get props Padding
            }
        StandardLayouts.standardLayout props layoutCache childLayout

    let rec elementInfo instance = {
        state = instance.state
        props = instance.props
        behavior = instance.behavior
        getParent = fun () -> instance.parent |> Option.map elementInfo
        getChildren = fun prop ->
            instance.children
            |> ImmutableDictionary.tryFind prop
            |> Option.defaultValue []
            |> List.map elementInfo
    }
        
module Zero =
    let zeroState = { new IElementState }
    let behavior = {
        name = "Zero"
        mount = fun _ -> zeroState
        unmount = ignore
        render = fun _ -> id
        layout = fun _ -> Layman.Layouts.DefaultLayouts.zero
    }
    let zero = {
        behavior = behavior
        props = PropertyBag.Empty
    }

module Border =
    let Child = declareProperty "Child" Zero.zero
    let Background = declareProperty "Background" (SolidColorBrush Color.Magenta)

    type State =
        {
            layoutCache: LayoutCache
        }
        interface IElementState

    let mount _ = { layoutCache = LayoutCache.create() } :> IElementState

    let layout (elem: ElementInfo) =
        let state = elem.state :?> State
        let childLayout =
            match elem.getChildren Child with
            | child::_ -> child.behavior.layout child
            | [] -> zero
        standardLayout elem.props state.layoutCache childLayout

    let render (elem: ElementInfo) =
        let childRender =
            match elem.getChildren Child with
            | child :: _ -> child.behavior.render child
            | [] -> id

        let bounds = (elem.state :?> State).layoutCache.relativeBounds

        VisualLayer.pushVisual {
            Visual.empty with
                offset = Vector2(float32 bounds.X, float32 bounds.Y)
                size = Vector2(float32 bounds.Width, float32 bounds.Height)
                brush = get elem.props Background
        }
        >> childRender
        >> VisualLayer.pop

    let behavior = {
        name = "Border"
        mount = mount
        unmount = ignore
        render = render
        layout = layout
    }

    let border props = {
        behavior = behavior
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
        | NoDiff of ElementInstance * Element
        | Update of ElementInstance * Element
        | Unmount of ElementInstance
        | Mount of Element

    let diffChildren (instances: ElementInstance seq) descriptions =
        let elementKey i props = get props Key |> Option.defaultValue (i :> IComparable)
        let keyedDescriptions = descriptions |> Seq.mapi (fun i desc -> elementKey i desc.props, desc)
        let keyedInstances = instances |> Seq.mapi (fun i inst -> elementKey i inst.props, inst)

        // Do a full outer join to...
        // (1) match elements with existing element instances and
        // (2) find elements that need to be newly instantiated
        // (3) find instances that are no longer part of virtual tree, hence must be unmounted
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
    
    let diffChildrenAllProps (instances: ElementInstanceBag) node =
        // Find props holding a single child element and recursively instantiate them
        let singleChildProps =
            node.props
            |> Seq.filter (fun (KeyValuePair(key, _)) -> key.propType = typedefof<Element>)
            |> Seq.map (fun (KeyValuePair(key, node)) -> key, Seq.singleton (node :?> Element))

        // Find props holding collections of child elements and recursively instantiate them
        let multiChildProps = 
            node.props
            |> Seq.filter (fun (KeyValuePair(key, _)) -> typedefof<Element seq>.IsAssignableFrom key.propType)
            |> Seq.map (fun (KeyValuePair(key, nodes)) -> key, (nodes :?> Element seq))
            |> Seq.filter (fun (_, nodes) -> not (Seq.isEmpty nodes))

        Seq.append singleChildProps multiChildProps
        |> Seq.map (fun (prop, descriptions) ->
            let instances = ImmutableDictionary.tryFind prop instances |> Option.defaultValue []
            prop, (diffChildren instances descriptions))

    let rec updateChildrenAllProps instances node parent =
        let handleProp (prop, diffs) =
            let instances =
                diffs
                |> Seq.map (function
                    | NoDiff(inst, _) -> Some inst
                    | Update(inst, desc) -> Some (updateSubtree inst desc parent)
                    | Mount desc -> Some (mountSubtree desc parent)
                    | Unmount inst -> unmountSubtree inst; None)
                |> Seq.filter (fun inst -> inst.IsSome)
                |> Seq.map (fun inst -> inst.Value)
                |> List.ofSeq
            prop, instances

        diffChildrenAllProps instances node
        |> Seq.map handleProp
        |> ImmutableDictionary.ofSeq

    and unmountSubtree (tOld: ElementInstance) =
        tOld.behavior.unmount (elementInfo tOld)
        tOld.children |> ImmutableDictionary.iter (fun _ -> Seq.iter unmountSubtree)

    and mountSubtree (tNew: Element) parent =
        let state = tNew.behavior.mount tNew.props
        let instance = {
            behavior = tNew.behavior
            parent = parent
            props = tNew.props
            children = ElementInstanceBag.Empty
            state = state
        }
        instance.children <- updateChildrenAllProps ElementInstanceBag.Empty tNew (Some instance)
        instance

    and updateSubtree (tOld: ElementInstance) (tNew: Element) parent =
        // compare old and new:
        // if behavior and props are identical, stop recursion (subtree doesn't change)
        // otherwise, update props and recurse
        if tNew.behavior = tOld.behavior && tNew.props = tOld.props then
            tOld
        else
            let instance = {
                behavior = tOld.behavior // tNew has same behavior
                parent = parent
                children = ElementInstanceBag.Empty // assigned below
                props = tNew.props
                state = tOld.state
            }
            instance.children <- updateChildrenAllProps tOld.children tNew (Some instance)
            instance

    // After a UI tree (or a part of it) was (re-)rendered, use this function
    // to determine and apply changes to the actual element instances.
    let update (tOld: ElementInstance option) (tNew: Element) parent =
        match tOld with
        | None -> mountSubtree tNew parent
        | Some tOld -> updateSubtree tOld tNew parent

module UISample =
    let sample() =
        Border.border [
            UIElement.HorizontalAlignment @= Alignment.End
            UIElement.Width @= 100.0
            UIElement.Height @= 100.0
            Border.Background @= SolidColorBrush Color.Red
            Border.Child @= Border.border [
                UIElement.Height @= 20.0
                UIElement.Margin @= Thickness.createUniform 4.0
                UIElement.VerticalAlignment @= Alignment.Center
                Border.Background @= SolidColorBrush Color.DeepSkyBlue
            ]
        ]