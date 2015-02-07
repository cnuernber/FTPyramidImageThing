// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Gtk
open System
open Glade

type private GladeObj () =
    [<Widget>] [<DefaultValue>] val mutable mainwindow : Window
    [<Widget>] [<DefaultValue>] val mutable rotation : Entry
    [<Widget>] [<DefaultValue>] val mutable scale : Entry
    [<Widget>] [<DefaultValue>] val mutable translation : Entry
    [<Widget>] [<DefaultValue>] val mutable source : Entry
    [<Widget>] [<DefaultValue>] val mutable destination : Entry
    [<Widget>] [<DefaultValue>] val mutable renderview : Viewport
    [<Widget>] [<DefaultValue>] val mutable resultview : Viewport

    


[<EntryPoint>]
let main argv = 
    Application.Init()
    let gxml = new Glade.XML( null, "gui.glade", "mainwindow", null )
    let gobj = new GladeObj()
    gxml.Autoconnect( gobj )


    Application.Run()
    0 // return an integer exit code
