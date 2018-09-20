namespace OrangeBug.DesktopClient.LibUI

open System
open System.Collections.Generic
open Layman
open Layman.Layouts
open OrangeBug.DesktopClient
open Microsoft.Xna.Framework
open System.Collections.Immutable
open System.Threading
open Microsoft.Xna.Framework.Input

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
    // TODO: Static type checking is desirable for property assignments
    static member (@=) (key: PropertyKey, value: 'a) =
        if key.propType.IsAssignableFrom typedefof<'a>
        then KeyValuePair(key, value :> obj)
        else failwithf "Cannot assign value of type '%s' to property '%s' of type '%s'" (value.GetType().Name) key.name key.propType.Name

type PropertyBag = ImmutableDictionary<PropertyKey, obj>

and ElementInfo = {
    behavior: ElementBehavior
    props: PropertyBag
    state: IElementState
    getParent: unit -> ElementInfo option
    getChildren: unit -> ElementInfo list
    invalidate: unit -> unit
}

and [<ReferenceEquality; StructuredFormatDisplay("{name}")>] ElementBehavior = {
    name: string
    mount: PropertyBag -> (unit -> unit) -> IElementState // computes the initial state when a new instance of this element is created
    unmount: ElementInfo -> unit // performs any cleanup tasks when the element is destroyed
    draw: ElementInfo -> VisualLayer.State -> VisualLayer.State // is called at 60 fps to draw the UI
    layout: ElementInfo -> LayoutPhase -> LayoutResult
    render: ElementInfo -> Element list
}

// this is only used to construct our "virtual DOM"
and Element = {
    behavior: ElementBehavior
    props: PropertyBag
}

and [<ReferenceEquality>] ElementInstance = {
    behavior: ElementBehavior
    parent: ElementInstance option // actual parent in fully rendered tree
    mutable children: ElementInstance list // actual children in fully rendered tree
    mutable props: PropertyBag
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

[<AutoOpen>]
module UIElement =
    type Vector2 = Microsoft.Xna.Framework.Vector2 // to avoid clashes with Layman's Vector2
    let layvec2 = Layman.BasicConstructors.vec2
    let nullState = { new IElementState }

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
        >> fun r -> LayoutCache.invalidateMeasure layoutCache; r // DEBUG: Do not cache layout results

    let defaultLayout (elem: ElementInfo) =
        overlay (elem.getChildren() |> List.map (fun child -> child.behavior.layout child))
    
    let defaultDraw (elem: ElementInfo) (context: VisualLayer.State) =
        elem.getChildren() |> List.fold (fun ctx child -> child.behavior.draw child ctx) context

    let defaultBehavior name = {
        name = name
        mount = (fun _ _ -> nullState)
        unmount = ignore
        layout = defaultLayout
        draw = defaultDraw
        render = fun _ -> []
    }

    let rec elementInfo invalidate instance = {
        state = instance.state
        props = instance.props
        behavior = instance.behavior
        getChildren = fun () -> instance.children |> List.map (elementInfo invalidate)
        getParent = fun () -> instance.parent |> Option.map (elementInfo invalidate)
        invalidate = invalidate instance
    }
        
module Zero =
    let behavior = defaultBehavior "Zero"
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

    let mount _ _ = { layoutCache = LayoutCache.create() } :> IElementState

    let layout (elem: ElementInfo) =
        let state = elem.state :?> State
        let childLayout =
            match elem.getChildren() with
            | child::_ -> child.behavior.layout child
            | _ -> zero
        standardLayout elem.props state.layoutCache childLayout

    let draw (elem: ElementInfo) =
        let childDraw =
            match elem.getChildren() with
            | child::_ -> child.behavior.draw child
            | _ -> id

        let bounds = (elem.state :?> State).layoutCache.relativeBounds

        VisualLayer.pushVisual {
            Visual.empty with
                offset = Vector2(float32 bounds.X, float32 bounds.Y)
                size = Vector2(float32 bounds.Width, float32 bounds.Height)
                brush = get elem.props Background
        }
        >> childDraw
        >> VisualLayer.pop

    let behavior = {
        defaultBehavior "Border" with
            mount = mount
            draw = draw
            layout = layout
            render = fun elem -> [get elem.props Child]
    }

    let border props = {
        behavior = behavior
        props = toPropertyBag props
    }

module UITree =
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

    let diffChildren (instances: ElementInstance seq) (descriptions: Element seq) =
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
    
    let rec unmountSubtree (tOld: ElementInstance) =
        let dummyInvalidate _ () = () // invalidate has no effect during unmount
        tOld.behavior.unmount (elementInfo dummyInvalidate tOld)
        tOld.children |> List.iter unmountSubtree

    let rec mountSubtree parent invalidate (tNew: Element) =
        let instance = {
            behavior = tNew.behavior
            parent = parent
            props = tNew.props
            state = Unchecked.defaultof<IElementState>
            children = []
        }
        instance.state <- tNew.behavior.mount tNew.props (invalidate instance)
        instance.children <-
            tNew.behavior.render (elementInfo invalidate instance)
            |> List.map (mountSubtree (Some instance) invalidate)
        instance

    and updateSubtree parent invalidate (tOld: ElementInstance) (tNew: Element) =
        // compare old and new:
        // if behavior and props are identical, stop recursion (subtree doesn't change)
        // otherwise, update props and recurse
        if tNew.behavior = tOld.behavior && tNew.props = tOld.props then
            tOld
        else
            let instance = {
                behavior = tOld.behavior // tNew has same behavior
                parent = parent
                props = tNew.props
                state = tOld.state
                children = []
            }
            instance.children <-
                tNew.behavior.render (elementInfo invalidate instance)
                |> updateChildren instance invalidate tOld.children 
            instance

    and updateChildren parent invalidate oldChildren newChildren =
        let handleDiff = function
            | NoDiff(inst, _) -> Some inst
            | Update(inst, desc) -> Some (updateSubtree (Some parent) invalidate inst desc)
            | Mount(desc) -> Some (mountSubtree (Some parent) invalidate desc)
            | Unmount(inst) -> unmountSubtree inst; None
        diffChildren oldChildren newChildren
        |> Seq.choose handleDiff
        |> List.ofSeq

type UI = {
    mutable root: ElementInstance
    mutable dirtyInstances: ImmutableQueue<ElementInstance>
}

module UI =
    let invalidate ui instance () =
        if not (ui.dirtyInstances |> Seq.contains instance) then
            ui.dirtyInstances <- ui.dirtyInstances.Enqueue instance
    
    let create root =
        let ui = {
            root = Unchecked.defaultof<ElementInstance>
            dirtyInstances = ImmutableQueue.Empty
        }
        ui.root <- UITree.mountSubtree None (invalidate ui) root
        ui

    let update ui =
        let re'render instance =
            let ancestors = Seq.unfold (fun inst -> inst.parent |> Option.map (fun p -> p, p)) instance
            if ancestors |> Seq.exists (fun a -> ui.dirtyInstances |> Seq.contains a) then
                () // found a dirty ancestor - re-rendering is useless
            else
                let invalidate = invalidate ui
                let newChildren = instance.behavior.render (elementInfo invalidate instance)
                let updatedChildInstances = UITree.updateChildren instance invalidate instance.children newChildren
                instance.children <- updatedChildInstances

        while not ui.dirtyInstances.IsEmpty do
            let remainingDirty, instance = ui.dirtyInstances.Dequeue()
            re'render instance
            ui.dirtyInstances <- remainingDirty


module CustomElementSample =
    let IsInitiallyOn = declareProperty "IsInitiallyOn" true

    type State = {
        mutable timer: Timer option
        mutable isOn: bool
    } with interface IElementState

    let mount props invalidate =
        let state = {
            isOn = get props IsInitiallyOn
            timer = None
        }
        let tick _ =
            if Keyboard.GetState().IsKeyDown(Keys.Space) then
                state.isOn <- not state.isOn
                invalidate()
        state.timer <- Some (new Timer(tick, null, 16, 16))
        state :> IElementState

    let unmount (elem: ElementInfo) =
        (elem.state :?> State).timer.Value.Dispose()
    
    let render (elem: ElementInfo) =
        let state = (elem.state :?> State)
        [
            Border.border [
                UIElement.Margin @= Thickness.createUniform 20.0
                UIElement.Padding @= Thickness.createUniform 2.0
                Border.Background @= SolidColorBrush Color.Cyan
                Border.Child @=
                    if state.isOn
                    then Border.border [ Border.Background @= SolidColorBrush Color.Green ]
                    else Border.border [ Border.Background @= SolidColorBrush Color.IndianRed; UIElement.Height @= 40.0 ]
            ]
        ]

    let behavior = {
        defaultBehavior "CustomElementSample" with
            mount = mount
            render = render
    }

    let customElementSample props = {
        behavior = behavior
        props = toPropertyBag props
    }

module UISample =
    let sample() =
        Border.border [
            UIElement.HorizontalAlignment @= Alignment.End
            UIElement.Width @= 100.0
            UIElement.Height @= 100.0
            Border.Background @= SolidColorBrush Color.Red
            Border.Child @= CustomElementSample.customElementSample [
                UIElement.Margin @= Thickness.createUniform 4.0
            ]
        ]