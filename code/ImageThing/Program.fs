// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Gtk
open System
open Glade

type private GladeObj =
    [<Widget>] val mutable button1 : Button
    [<Widget>] val mutable label1 : Label
    new() = { button1 = null; label1 = null; }


[<EntryPoint>]
let main argv = 
    Application.Init()
    let gxml = new Glade.XML( null, "gui.glade", "window1", null )
    let gobj = new GladeObj()
    gxml.Autoconnect( gobj )
    gobj.button1.Clicked.Add( fun evArgs -> 
                                Console.WriteLine( "Shit got pressed") 
                                gobj.label1.Text <- "Mutable FSharp")


    Application.Run()
    0 // return an integer exit code
