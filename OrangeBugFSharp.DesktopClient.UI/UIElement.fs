namespace OrangeBug.DesktopClient.LibUI

open OrangeBug.DesktopClient
open Layman
open Layman.Layouts

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

    let layout (elem: ElementInfo) =
        let size = TextRendering.measure (get elem.props Text)
        standardLayout elem.props elem.layoutCache (fixedSize (SysVector2(size.X, size.Y)) zero)

    let draw (elem: ElementInfo) (context: VisualLayer.State) =
        let bounds = elem.layoutCache.relativeBounds
        let tex = TextRendering.renderToTexture (get elem.props Text) (bounds.size.AsXna()) context.graphicsDevice

        context
        |> VisualLayer.pushVisual {
            Visual.empty with
                offset = bounds.offset.AsXna()
                size = bounds.size.AsXna()
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

