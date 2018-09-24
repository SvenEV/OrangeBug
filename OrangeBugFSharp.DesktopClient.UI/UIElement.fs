namespace OrangeBug.DesktopClient.LibUI

open OrangeBug.DesktopClient
open Layman
open Layman.Layouts
open Microsoft.Xna.Framework.Graphics

[<AutoOpen>]
module UIElement =
    let nullState = { new IElementState }

    let Width = PropertyKey.register "Width" nanf AffectsMeasure
    let Height = PropertyKey.register "Height" nanf  AffectsMeasure
    let MinWidth = PropertyKey.register "MinWidth" 0.0f  AffectsMeasure
    let MinHeight = PropertyKey.register "MinHeight" 0.0f  AffectsMeasure
    let MaxWidth = PropertyKey.register "MaxWidth" infinityf  AffectsMeasure
    let MaxHeight = PropertyKey.register "MaxHeight" infinityf  AffectsMeasure
    let Margin = PropertyKey.register "Margin" Thickness.zero  AffectsMeasure
    let Padding = PropertyKey.register "Padding" Thickness.zero  AffectsMeasure
    let HorizontalAlignment = PropertyKey.register "HorizontalAlignment" Alignment.Stretch AffectsArrange
    let VerticalAlignment = PropertyKey.register "VerticalAlignment" Alignment.Stretch AffectsArrange

    let standardLayout props (layoutCache: LayoutCache) childLayout =
        let props =
            { StandardLayouts.defaultProps with
                size = SysVector2(get props Width, get props Height)
                minSize = SysVector2(get props MinWidth, get props MinHeight)
                maxSize = SysVector2(get props MaxWidth, get props MaxHeight)
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
        
module Zero =
    let behavior = defaultBehavior "Zero"
    let zero = {
        behavior = behavior
        props = PropertyBag.Empty
    }

module Border =
    let Child = PropertyKey.register "Child" Zero.zero AffectsMeasure
    let Background = PropertyKey.register "Background" (SolidColorBrush XnaColor.Magenta) NoLayoutEffect

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
                offset = XnaVector2(bounds.X, bounds.Y)
                size = XnaVector2(bounds.Width, bounds.Height)
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
    let FontFamily = PropertyKey.register "FontFamily" "Arial" AffectsMeasure
    let FontSize = PropertyKey.register "FontSize" 12.0f AffectsMeasure
    let FontStyle = PropertyKey.register "FontStyle" SixLabors.Fonts.FontStyle.Regular AffectsMeasure
    let TextWrapping = PropertyKey.register "TextWrapping" TextWrapping.Wrap AffectsMeasure
    let TextAlignment = PropertyKey.register "TextAlignment" TextAlignment.Left AffectsMeasure

    type State = {
        mutable texture: Texture2D option
    } with interface IElementState

    let createTextOptions props space = {
        fontFamily = get props FontFamily
        fontSize = get props FontSize
        fontStyle = get props FontStyle
        textWrapping = get props TextWrapping
        textAlignment = get props TextAlignment
        constraintSize = space
    }

    let propsChanged (elem: ElementInfo) (oldProps: PropertyBag) =
        UIElement.defaultPropsChanged elem oldProps
        (elem.state :?> State).texture <- None

    let layout (elem: ElementInfo) =
        // TODO: Hm, this is called in every frame to construct the "layout tree" even though layouting
        // itself does not happen every frame due to caching. Could we solve this by passing child layouts as thunks?
        let childLayout = function
            | Measure(space, _) ->
                (elem.state :?> State).texture <- None
                let options = createTextOptions elem.props (space.AsXna())
                let size = (TextRendering.measure (get elem.props Text) options).AsSys()
                Measured (size, ObjTree.leaf size)
            | Arrange(rect, Tree(:? SysVector2 as measuredSize, _), _) ->
                Arranged rect
        
        standardLayout elem.props elem.layoutCache childLayout


    let draw (elem: ElementInfo) (context: VisualLayer.State) =
        let bounds = elem.layoutCache.relativeBounds
        let state = elem.state :?> State

        let texture =
            match state.texture with
            | Some tex -> tex
            | None ->
                let options = createTextOptions elem.props (bounds.size.AsXna())
                let tex = TextRendering.renderToTexture (get elem.props Text) options context.graphicsDevice
                state.texture <- Some tex
                tex

        context
        |> VisualLayer.pushVisual {
            Visual.empty with
                offset = bounds.offset.AsXna()
                size = bounds.size.AsXna()
                brush = TextureBrush texture 
        }
        |> VisualLayer.pop

    let behavior = {
        defaultBehavior "TextBlock" with
            mount = fun _ _ -> { texture = None } :> IElementState
            layout = layout
            draw = draw
            propsChanged = propsChanged
    }

    let textBlock props = {
        behavior = behavior
        props = PropertyBag(props)
    }

