namespace OrangeBug.DesktopClient.LibUI

open Layman
open System.Threading
open OrangeBug.DesktopClient
open Microsoft.Xna.Framework.Input
open SixLabors.Fonts

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
                UIElement.Margin @= Thickness.createUniform 20.0f
                UIElement.Padding @= Thickness.createUniform 2.0f
                UIElement.HorizontalAlignment @= if state.center then Stretch else Start
                Border.Background @= SolidColorBrush XnaColor.Cyan
                Border.Child @=
                    if state.isOn
                    then TextBlock.textBlock [
                        TextBlock.Text @= "Hello World!\r\nWe can go MULTILINE, too :)\r\nLet's see what happens with very long lines like this one which has many words."
                        TextBlock.TextAlignment @= Center
                        TextBlock.FontSize @= 20.0f
                        TextBlock.FontStyle @= FontStyle.Bold ]
                    else Border.border [ Border.Background @= SolidColorBrush XnaColor.IndianRed; UIElement.Height @= 80.0f ]
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
            UIElement.Width @= 400.0f
            UIElement.Height @= 400.0f
            Border.Background @= SolidColorBrush XnaColor.Red
            Border.Child @= CustomElementSample.customElementSample [
                UIElement.Margin @= Thickness.createUniform 4.0f
            ]
        ]