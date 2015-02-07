// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Gtk
open System
open Glade
open OpenTK
open OpenTK.Graphics.OpenGL

type private GladeObj () =
    [<Widget>] [<DefaultValue>] val mutable mainwindow : Window
    [<Widget>] [<DefaultValue>] val mutable rotation : Entry
    [<Widget>] [<DefaultValue>] val mutable scale : Entry
    [<Widget>] [<DefaultValue>] val mutable translation : Entry
    [<Widget>] [<DefaultValue>] val mutable source : Entry
    [<Widget>] [<DefaultValue>] val mutable destination : Entry
    [<Widget>] [<DefaultValue>] val mutable renderview : Viewport
    [<Widget>] [<DefaultValue>] val mutable resultview : Viewport
    [<Widget>] [<DefaultValue>] val mutable destBrowse : Button
    [<Widget>] [<DefaultValue>] val mutable srcBrowse : Button

    


[<EntryPoint>]
let main argv = 
    Application.Init()
    let gxml = new Glade.XML( null, "gui.glade", "mainwindow", null )
    let gobj = new GladeObj()
    gxml.Autoconnect( gobj )
    let renderer = new GLWidget()
    let results = new Image()
    gobj.renderview.Add( renderer )

    let renderFrame evArgs = 
        let mutable width = 0
        let mutable height = 0
        gobj.renderview.GetSizeRequest(ref width, ref height)
        GL.Viewport( 0, 0, width, height)
        GL.ClearColor( 1.0f, 0.0f, 0.0f, 1.0f )
        GL.Clear( ClearBufferMask.ColorBufferBit )


    let toolkit = Toolkit.Init()
    renderer.RenderFrame.Add( renderFrame )
    gobj.renderview.ShowAll();
    gobj.resultview.Add( results )
    gobj.resultview.ShowAll();
    Application.Run()
    0 // return an integer exit code
