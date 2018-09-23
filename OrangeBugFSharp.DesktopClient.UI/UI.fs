namespace OrangeBug.DesktopClient.LibUI

open Layman
open System
open System.Collections.Immutable
open OrangeBug.DesktopClient

type IElementState = interface end

type ElementInfo = {
    behavior: ElementBehavior
    props: PropertyBag
    state: IElementState
    layoutCache: LayoutCache
    getParent: unit -> ElementInfo option
    getChildren: unit -> ElementInfo list
    invalidate: unit -> unit
}

and [<ReferenceEquality; StructuredFormatDisplay("{name}")>] ElementBehavior = {
    name: string
    mount: PropertyBag -> (unit -> unit) -> IElementState // computes the initial state when a new instance of this element is created
    unmount: ElementInfo -> unit // performs any cleanup tasks when the element is destroyed
    propsChanged: ElementInfo -> PropertyBag -> unit
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
    layoutCache: LayoutCache
    mutable children: ElementInstance list // actual children in fully rendered tree
    mutable props: PropertyBag
    mutable state: IElementState
}

[<AutoOpen>]
module ElementInstance =
    let Key = PropertyKey.register "Key" (None: IComparable option) NoLayoutEffect
    let rec elementInfo invalidate instance = {
        state = instance.state
        props = instance.props
        behavior = instance.behavior
        layoutCache = instance.layoutCache
        getChildren = fun () -> instance.children |> List.map (elementInfo invalidate)
        getParent = fun () -> instance.parent |> Option.map (elementInfo invalidate)
        invalidate = invalidate instance
    }

module UITree =
    type Diff =
        | NoDiff of ElementInstance * Element
        | Update of ElementInstance * Element
        | Unmount of ElementInstance
        | Mount of Element

    let rec unmountSubtree instance =
        let dummyInvalidate _ () = () // invalidate has no effect during unmount
        instance.behavior.unmount (elementInfo dummyInvalidate instance)
        instance.children |> List.iter unmountSubtree

    let rec mountSubtree parent invalidate (description: Element) =
        let instance = {
            behavior = description.behavior
            parent = parent
            layoutCache = LayoutCache.create()
            props = description.props
            state = Unchecked.defaultof<IElementState>
            children = []
        }
        instance.state <- description.behavior.mount description.props (invalidate instance)
        instance.children <-
            description.behavior.render (elementInfo invalidate instance)
            |> List.map (mountSubtree (Some instance) invalidate)
        instance

    let diff (inst: ElementInstance option) (desc: Element option) =
        match inst, desc with
        | None, None -> failwith "This should not happen"
        | None, Some desc -> [Mount desc]
        | Some inst, None -> [Unmount inst]
        | Some inst, Some desc when inst.behavior = desc.behavior ->
            // If behavior and props are identical, no need to rerender element (recursion stops)
            // (caution: 'PropertyBag' is an 'ImmutableDictionary' which only has reference equality)
            if inst.props = desc.props
            then [NoDiff(inst, desc)]
            else [Update(inst, desc)]
        | Some inst, Some desc ->
            // If behavior changed, unmount old instance and mount new instance
            [ Unmount inst; Mount desc ]

    let diffChildren (instances: ElementInstance seq) (descriptions: Element seq) =
        // Do a full outer join to...
        // (1) match elements with existing element instances and
        // (2) find elements that need to be newly instantiated
        // (3) find instances that are no longer part of virtual tree, hence must be unmounted
        let elementKey i props = get props Key |> Option.defaultValue (i :> IComparable)
        let keyedDescriptions = descriptions |> Seq.mapi (fun i desc -> elementKey i desc.props, desc)
        let keyedInstances = instances |> Seq.mapi (fun i inst -> elementKey i inst.props, inst)
        (keyedInstances, keyedDescriptions)
        ||> Seq.fullOuterJoin fst fst (fun inst desc _ -> diff (Option.map snd inst) (Option.map snd desc))
    
    let rec rerenderSubtree invalidate instance =
        let handleDiff = function
            | Mount(desc) -> mountSubtree (Some instance) invalidate desc |> Some
            | Unmount(inst) -> unmountSubtree inst; None
            | NoDiff(inst, _) -> Some inst
            | Update(inst, desc) ->
                let oldProps = inst.props
                inst.props <- desc.props
                inst.behavior.propsChanged (elementInfo invalidate inst) oldProps
                rerenderSubtree invalidate inst
                Some inst
        instance.children <-
            instance.behavior.render (elementInfo invalidate instance)
            |> diffChildren instance.children
            |> Seq.collect id
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
        let rerender instance =
            // Re-render unless there's a dirty ancestor - in that case re-rendering is useless
            let ancestors = Seq.unfold (fun inst -> inst.parent |> Option.map (fun p -> p, p)) instance
            if not (ancestors |> Seq.exists (fun a -> ui.dirtyInstances |> Seq.contains a)) then
                UITree.rerenderSubtree (invalidate ui) instance

        while not ui.dirtyInstances.IsEmpty do
            let remainingDirty, instance = ui.dirtyInstances.Dequeue()
            rerender instance
            ui.dirtyInstances <- remainingDirty
