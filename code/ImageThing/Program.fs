// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Gtk
open System
open System.Xml
open System.IO
open System.Xml.Serialization
open Glade
open OpenTK
open OpenTK.Graphics.OpenGL

type public GladeObj () =
    [<Widget>] [<DefaultValue>] val mutable mainwindow : Window
    [<Widget>] [<DefaultValue>] val mutable rotation : Entry
    [<Widget>] [<DefaultValue>] val mutable scale : Entry
    [<Widget>] [<DefaultValue>] val mutable translation : Entry
    [<Widget>] [<DefaultValue>] val mutable source : Entry
    [<Widget>] [<DefaultValue>] val mutable destination : Entry
    [<Widget>] [<DefaultValue>] val mutable destsize : Entry
    [<Widget>] [<DefaultValue>] val mutable renderview : Viewport
    [<Widget>] [<DefaultValue>] val mutable resultview : Viewport
    [<Widget>] [<DefaultValue>] val mutable destBrowse : Button
    [<Widget>] [<DefaultValue>] val mutable srcBrowse : Button

type public ModelData() =
    [<DefaultValue>] val mutable rotation : float32
    [<DefaultValue>] val mutable scale : float32
    [<DefaultValue>] val mutable translation : Vector2
    [<DefaultValue>] val mutable source : string
    [<DefaultValue>] val mutable destination : string
    [<DefaultValue>] val mutable destsize : int

    static member defaults() = 
      let retval = new ModelData()
      retval.rotation <- 0.0f
      retval.scale <- 1.0f
      retval.translation <- Vector2()
      retval.source <- ""
      retval.destination <- ""
      retval.destsize <- 100
      retval


    static let appDir = 
        let localsDir = Environment.GetFolderPath( Environment.SpecialFolder.LocalApplicationData )
        Path.Combine( localsDir, "FTImageThing")
        
    static let settingsFileName =
        Path.Combine( appDir, "settings.xml")

    static member ensureAppDir() =
        ignore( Directory.CreateDirectory(appDir) )
        

    static member load () =
        ModelData.ensureAppDir()
        try
            let file = File.OpenRead( settingsFileName )
            try
                let mytype = typeof<ModelData>
                let serializer = XmlSerializer(mytype)
                serializer.Deserialize(file) :?> ModelData
            finally
                file.Close()
        with
         | except -> ModelData.defaults()

    member this.save() = 
        ModelData.ensureAppDir()
        try
            let file = File.OpenWrite( settingsFileName )
            try
                let mytype = typeof<ModelData>
                let serializer = XmlSerializer( mytype )
                ignore( serializer.Serialize( file, this ) )
            finally
                file.Close()
        with
            | except -> System.Diagnostics.Debug.Print(except.ToString())
    
        
    member this.toUI(ui:GladeObj) =
        ui.rotation.Text <- this.rotation.ToString()
        ui.scale.Text <- this.scale.ToString()
        ui.translation.Text <- String.Concat(this.translation.X.ToString(), " ", this.translation.Y.ToString() )
        ui.source.Text <- this.source.ToString()
        ui.destination.Text <- this.destination.ToString()
        ui.destsize.Text <- this.destsize.ToString()

    static member parseFloat( data, existing ) =
        let (ok, data)  = System.Double.TryParse( data )
        if ok then  float32(data) else existing

    static member parseInt( data, existing ) =
        let (ok, data)  = System.Int32.TryParse( data )
        if ok then  data else existing

    static member parseVec2( data:string, existing ) =
        try
            let parts = data.Split( [|' '; ','|])
            let x = ModelData.parseFloat( parts.[0], 0.0f )
            let y = ModelData.parseFloat( parts.[1], 0.0f )
            Vector2( x, y )
        with
            | _ -> existing

    member this.fromUI( ui:GladeObj) =
        this.rotation <- ModelData.parseFloat( ui.rotation.Text, this.rotation )
        this.scale <- ModelData.parseFloat( ui.scale.Text, this.scale )
        this.translation <- ModelData.parseVec2( ui.translation.Text, this.translation )
        this.source <- ui.source.Text
        this.destination <- ui.destination.Text
        this.destsize <- ModelData.parseInt( ui.destsize.Text, this.destsize )


[<EntryPoint>]
let main argv = 
    Application.Init()
    let gxml = new Glade.XML( null, "gui.glade", "mainwindow", null )
    let gobj = new GladeObj()
    gxml.Autoconnect( gobj )
    let renderer = new GLWidget()
    let results = new Image()
    gobj.renderview.Add( renderer )
    let settings = ModelData.load()

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
    settings.toUI(gobj)
    gobj.mainwindow.Destroyed.Add( fun evArgs -> settings.fromUI(gobj); settings.save(); Application.Quit() )
    Application.Run()
    0 // return an integer exit code
