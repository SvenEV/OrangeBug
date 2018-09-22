namespace OrangeBug.DesktopClient.LibUI

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Linq
open System.Threading
open Layman
open Layman.Layouts
open OrangeBug.DesktopClient
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type IElementState = interface end

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
module PropertySystem =
    let get (props: PropertyBag) key = props.get key

    let diffProps (oldProps: PropertyBag) (newProps: PropertyBag) =
        (oldProps, newProps)
        ||> Seq.fullOuterJoin fst fst (fun a b key -> if a = b then None else Some key)
        |> Seq.choose id

[<AutoOpen>]
module UIElement =
    type Vector2 = Microsoft.Xna.Framework.Vector2 // to avoid clashes with Layman's Vector2
    let layvec2 = Layman.BasicConstructors.vec2
    let nullState = { new IElementState }

    let Key = PropertyKey.register "Key" (None: IComparable option) NoLayoutEffect
    let Width = PropertyKey.register "Width" nan AffectsMeasure
    let Height = PropertyKey.register "Height" nan  AffectsMeasure
    let MinWidth = PropertyKey.register "MinWidth" 0.0  AffectsMeasure
    let MinHeight = PropertyKey.register "MinHeight" 0.0  AffectsMeasure
    let MaxWidth = PropertyKey.register "MaxWidth" infinity  AffectsMeasure
    let MaxHeight = PropertyKey.register "MaxHeight" infinity  AffectsMeasure
    let Margin = PropertyKey.register "Margin" Thickness.zero  AffectsMeasure
    let Padding = PropertyKey.register "Padding" Thickness.zero  AffectsMeasure
    let HorizontalAlignment = PropertyKey.register "HorizontalAlignment" Alignment.Stretch AffectsArrange
    let VerticalAlignment = PropertyKey.register "VerticalAlignment" Alignment.Stretch AffectsArrange

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

    let defaultPropsChanged (elem: ElementInfo) oldProps =
        let diff = diffProps oldProps elem.props
        if diff |> Seq.exists (fun key -> key.LayoutEffect = AffectsMeasure) then
            LayoutCache.invalidateMeasure elem.layoutCache
        else if diff |> Seq.exists (fun key -> key.LayoutEffect = AffectsArrange) then
            LayoutCache.invalidateArrange elem.layoutCache

    let defaultLayout (elem: ElementInfo) =
        overlay (elem.getChildren() |> List.map (fun child -> child.behavior.layout child))
    
    let defaultDraw (elem: ElementInfo) (context: VisualLayer.State) =
        elem.getChildren() |> List.fold (fun ctx child -> child.behavior.draw child ctx) context

    let defaultBehavior name = {
        name = name
        mount = (fun _ _ -> nullState)
        unmount = ignore
        propsChanged = defaultPropsChanged
        layout = defaultLayout
        draw = defaultDraw
        render = fun _ -> []
    }

    let rec elementInfo invalidate instance = {
        state = instance.state
        props = instance.props
        behavior = instance.behavior
        layoutCache = instance.layoutCache
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
    let Child = PropertyKey.register "Child" Zero.zero AffectsMeasure
    let Background = PropertyKey.register "Background" (SolidColorBrush Color.Magenta) NoLayoutEffect

    let layout (elem: ElementInfo) =
        let childLayout =
            match elem.getChildren() with
            | child::_ -> child.behavior.layout child
            | _ -> zero
        standardLayout elem.props elem.layoutCache childLayout

    let draw (elem: ElementInfo) =
        let childDraw =
            match elem.getChildren() with
            | child::_ -> child.behavior.draw child
            | _ -> id

        let bounds = elem.layoutCache.relativeBounds

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
            draw = draw
            layout = layout
            render = fun elem -> [get elem.props Child]
    }

    let border props = {
        behavior = behavior
        props = PropertyBag(props)
    }

module TextBlock =
    let Text = PropertyKey.register "Text" "" AffectsMeasure

    let layout (elem: ElementInfo) =
        let size = TextRendering2.measure (get elem.props Text)
        standardLayout elem.props elem.layoutCache (fixedSize (layvec2 (float size.X) (float size.Y)) zero)

    let draw (elem: ElementInfo) (context: VisualLayer.State) =
        let bounds = elem.layoutCache.relativeBounds
        let tex = TextRendering2.renderToTexture (get elem.props Text) (bounds.size.AsXna()) context.graphicsDevice

        context
        |> VisualLayer.pushVisual {
            Visual.empty with
                offset = Vector2(float32 bounds.X, float32 bounds.Y)
                size = Vector2(float32 bounds.Width, float32 bounds.Height)
                brush = TextureBrush tex 
        }
        |> VisualLayer.pop

    let behavior = {
        defaultBehavior "TextBlock" with
            layout = layout
            draw = draw
    }

    let textBlock props = {
        behavior = behavior
        props = PropertyBag(props)
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


module CustomElementSample =
    let IsInitiallyOn = PropertyKey.register "IsInitiallyOn" true NoLayoutEffect

    type State = {
        mutable timer: Timer option
        mutable isOn: bool
        mutable center: bool
    } with interface IElementState

    let mount props invalidate =
        let state = {
            isOn = get props IsInitiallyOn
            center = true
            timer = None
        }
        let tick _ =
            if Keyboard.GetState().IsKeyDown(Keys.Space) then
                state.isOn <- not state.isOn
                invalidate()
            if Keyboard.GetState().IsKeyDown(Keys.Enter) then
                state.center <- not state.center
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
                UIElement.HorizontalAlignment @= if state.center then Stretch else Start
                Border.Background @= SolidColorBrush Color.Cyan
                Border.Child @=
                    if state.isOn
                    then TextBlock.textBlock [ TextBlock.Text @= "Hello World!\r\nWe can go MULTILINE, too :)" ]
                    else Border.border [ Border.Background @= SolidColorBrush Color.IndianRed; UIElement.Height @= 80.0 ]
            ]
        ]

    let behavior = {
        defaultBehavior "CustomElementSample" with
            mount = mount
            render = render
    }

    let customElementSample props = {
        behavior = behavior
        props = PropertyBag(props)
    }

module UISample =
    let sample() =
        Border.border [
            UIElement.HorizontalAlignment @= Alignment.End
            UIElement.Width @= 400.0
            UIElement.Height @= 400.0
            Border.Background @= SolidColorBrush Color.Red
            Border.Child @= CustomElementSample.customElementSample [
                UIElement.Margin @= Thickness.createUniform 4.0
            ]
        ]