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
open System.Runtime.InteropServices
open System.IO

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

    let mutable ignoreUIEvents = false

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
            let file = File.Open( settingsFileName, FileMode.Create, FileAccess.Write )
            try
                let mytype = typeof<ModelData>
                let serializer = XmlSerializer( mytype )
                ignore( serializer.Serialize( file, this ) )
            finally
                file.Close()
        with
            | except -> System.Diagnostics.Debug.Print(except.ToString())
    
        
    member this.toUI(ui:GladeObj) =
        ignoreUIEvents <- true
        ui.rotation.Text <- this.rotation.ToString()
        ui.scale.Text <- this.scale.ToString()
        ui.translation.Text <- String.Concat(this.translation.X.ToString(), " ", this.translation.Y.ToString() )
        ui.source.Text <- this.source.ToString()
        ui.destination.Text <- this.destination.ToString()
        ui.destsize.Text <- this.destsize.ToString()
        ignoreUIEvents <- false

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
        if not ignoreUIEvents then
            this.rotation <- ModelData.parseFloat( ui.rotation.Text, this.rotation )
            this.scale <- ModelData.parseFloat( ui.scale.Text, this.scale )
            this.translation <- ModelData.parseVec2( ui.translation.Text, this.translation )
            this.source <- ui.source.Text
            this.destination <- ui.destination.Text
            this.destsize <- ModelData.parseInt( ui.destsize.Text, this.destsize )


let findFile( canRead, uiParent, existing:string ) =
    let (action,accept) = if canRead then (FileChooserAction.Open,"Open")
                            else (FileChooserAction.Save,"Save")

    let fc = new FileChooserDialog("Choose the file to open",
                                    uiParent,
                                    action,
                                    "Cancel",ResponseType.Cancel,
                                    accept,ResponseType.Accept)
    if not canRead then
        fc.DoOverwriteConfirmation <- true

    if existing.Length > 0 then
        ignore( fc.SetFilename( existing ) )

    if ( fc.Run() = int32(ResponseType.Accept) ) then
        Some(fc.Filename)
    else
        None

let bindModelToUI( ui:GladeObj, model:ModelData, render:GLWidget ) = 
    let genericHandler evArgs =
        model.fromUI(ui)
        render.QueueDraw()
    ui.rotation.Changed.Add( genericHandler )
    ui.scale.Changed.Add( genericHandler )
    ui.translation.Changed.Add( genericHandler )
    ui.destination.Changed.Add( genericHandler )
    ui.source.Changed.Add( genericHandler )
    ui.destsize.Changed.Add( genericHandler )
    ui.destBrowse.Clicked.Add( fun fnArgs -> 
                                    let opt = findFile( false, ui.mainwindow, model.destination)
                                    if opt.IsSome then
                                        model.destination <- opt.Value
                                        model.toUI(ui)
                                        render.QueueDraw() )
    ui.srcBrowse.Clicked.Add( fun fnArgs -> 
                                    let opt = findFile( true, ui.mainwindow, model.source)
                                    if opt.IsSome then
                                        model.source <- opt.Value
                                        model.toUI(ui)
                                        render.QueueDraw() )

let bgimage_vert = 
    "#version 130\n\
    in vec2 vpos;\n\
    out vec2 frag_vpos;\n\
    void main()\n\
    {\n\
    \tgl_Position = vec4(vpos.xy, 0.0, 1.0 );\n\
    \tfrag_vpos = vpos;\n\
     }\n"

let bgimage_frag =
    "#version 130\n\
    uniform sampler2D image;\n\
    in vec2 frag_vpos;\n\
    out vec4 fragData;\n\
    void main()\n\
    {\n\
    \tvec2 uvcoord = (frag_vpos + vec2(1.0, 1.0)) / 2.0;\n\
    \tvec4 texdata = texture( image, uvcoord );\n\
    \tfragData = texdata.xyzw;\n\
    }\n"

(*Uniforms have a location*)
type UniformData =
    struct
        val name : string
        val index : int
        val location : int
        val dtype : ActiveUniformType;
        new(_name,_index,_location, _dtype) 
            = { name = _name; index = _index; location = _location; dtype = _dtype; }
    end

let lookupUniforms( progHandle:int ) = 
    GL.UseProgram( progHandle )
    let mutable uniformCount = 0
    GL.GetProgram( progHandle, GetProgramParameterName.ActiveUniforms, &uniformCount )
    let ( retval : UniformData array ) = Array.zeroCreate uniformCount

    if uniformCount > 0 then
        for idx in 0..(uniformCount-1) do
            let mutable size = -1
            let mutable dtype = ActiveUniformType.Bool
            let name = GL.GetActiveUniform( progHandle, idx, &size, &dtype )
            let location = GL.GetUniformLocation( progHandle, name )
            retval.[idx] <- UniformData( name, idx, location, dtype )
    retval;

(*Attributes are always accessed via index*)
type AttribData =
    struct
        val name : string
        val index : int
        val size : int
        val dtype : ActiveAttribType;
        new(_name,_index, _size, _dtype) 
            = { name = _name; index = _index; size = _size; dtype = _dtype; }
    end

let lookupAttributes( progHandle:int ) = 
    GL.UseProgram( progHandle )
    let mutable attributeCount = 0;
    GL.GetProgram( progHandle, GetProgramParameterName.ActiveAttributes, &attributeCount )
    let ( retval : AttribData array ) = Array.zeroCreate attributeCount
    if attributeCount > 0 then
        for idx in 0..(attributeCount-1) do
            let mutable attribSize = -1;
            let mutable dtype = ActiveAttribType.Double
            let name = GL.GetActiveAttrib( progHandle, idx, &attribSize, &dtype )
            let loc = GL.GetAttribLocation( progHandle, name )
            retval.[idx] <- AttribData( name, idx, attribSize, dtype )
    retval

type Shader = 
    val progHandle : int;
    val uniforms : UniformData array
    val attributes : AttribData array
    new( hdl ) 
        = { progHandle = hdl; uniforms = lookupUniforms(hdl); attributes = lookupAttributes(hdl ) }

    member this.getUniLoc( name ) =
        let matches (uni : UniformData) = uni.name = name
        let item = Array.tryFind matches this.uniforms

        if item.IsSome then
            Some( item.Value.location )
        else
            None


        

let compileShader( code, stype ) =
    let hdl = GL.CreateShader( stype )
    GL.ShaderSource( hdl, code )
    GL.CompileShader(hdl)
    let mutable result = 0
    GL.GetShader(hdl, ShaderParameter.CompileStatus, &result)
    let log = GL.GetShaderInfoLog( hdl )
    if result = int(All.True) then
        if ( log.Length > 0 ) then
            Console.WriteLine( "Shader compilation output: ")
            Console.WriteLine( log )
        Some(hdl)
    else
        GL.DeleteShader( hdl )
        Console.WriteLine( "Failed to compile shader: " )
        Console.WriteLine( log )
        Console.WriteLine( code )
        None
        
type ShaderOpt = Shader option

let compileShaderProgram( vcode, fcode ) : Shader option= 
    let vhdl = compileShader( vcode, ShaderType.VertexShader )
    if vhdl.IsNone then ShaderOpt.None
    else

        let fhdl = compileShader(fcode, ShaderType.FragmentShader)
        if fhdl.IsNone then ShaderOpt.None
        else
            let phandle = GL.CreateProgram()
            GL.AttachShader( phandle, vhdl.Value )
            GL.AttachShader( phandle, fhdl.Value )
            GL.LinkProgram( phandle )
            (*Not needed after link*)
            GL.DeleteShader( vhdl.Value )
            GL.DeleteShader( fhdl.Value )
            let log = GL.GetProgramInfoLog( phandle )
            let mutable result = 0
            GL.GetProgram( phandle, GetProgramParameterName.LinkStatus, &result )
            
            if result = int(All.True) then
                if log.Length > 0 then
                    Console.WriteLine( "Program link output: ")
                    Console.WriteLine( log )

                Some(new Shader( phandle ))
            else
                Console.WriteLine( "Failed to link program: " )
                Console.WriteLine( log )
                Console.WriteLine( vcode )
                Console.WriteLine( fcode )
                None


                
let toNativef32 (data:float32[] ) =
    let datalen = data.Length
    let datasize = nativeint(datalen * 4);
    let retval = Marshal.AllocHGlobal( datasize )
    Marshal.Copy(data, 0, retval, data.Length )
    (retval,datasize)
    
let toNativei32 (data:int32[] ) =
    let datalen = data.Length
    let datasize = nativeint(datalen * 4);
    let retval = Marshal.AllocHGlobal( datasize )
    Marshal.Copy(data, 0, retval, data.Length )
    (retval,datasize)

let createAndUploadDataf32( data, target, usage ) =
    let retval = GL.GenBuffer()
    let (buffer,nativesize) = toNativef32( data )
    GL.BindBuffer( target, retval )
    GL.BufferData( target, nativesize, buffer, usage)
    Marshal.Release( buffer ) |> ignore
    retval
    
let createAndUploadDatai32( data, target, usage ) =
    let retval = GL.GenBuffer()
    let (buffer,nativesize) = toNativei32( data )
    GL.BindBuffer( target, retval )
    GL.BufferData( target, nativesize, buffer, usage)
    Marshal.Release( buffer ) |> ignore
    retval

type PathTexture =
    struct
        val handle : int
        val width : int
        val height : int
        val path : string
        new( hdl, w, h, p ) = { handle = hdl; width = w; height = h; path = p; }
    end
 
let createGlTexFromPath( path ) = 
    try
        let stream = new FileStream( path, FileMode.Open )
        let bm = new System.Drawing.Bitmap( stream )
        let format = bm.PixelFormat
        let bmdata = bm.LockBits( System.Drawing.Rectangle(0, 0, bm.Width, bm.Height)
                                    , System.Drawing.Imaging.ImageLockMode.ReadOnly
                                    , System.Drawing.Imaging.PixelFormat.Format32bppArgb)

        let length = bmdata.Stride * bm.Height
        let bytes : byte array = Array.zeroCreate length
        Marshal.Copy( bmdata.Scan0, bytes, 0, length)
        bm.UnlockBits( bmdata )
        (*flip the rows so the image is right-side up*)
        let row : byte array = Array.zeroCreate bmdata.Stride
        for h in 0..(bm.Height/2) do
            let endRow = bm.Height - h - 1
            let startOffset = h*bmdata.Stride
            let endOffset = endRow*bmdata.Stride
            let rowlen = bmdata.Stride
            Array.Copy(bytes, startOffset, row, 0, bmdata.Stride )
            Array.Copy(bytes, endOffset, bytes, startOffset, bmdata.Stride )
            Array.Copy(row, 0, bytes, endOffset, bmdata.Stride )

        let texhdl = GL.GenTexture()
        GL.ActiveTexture(TextureUnit.Texture0)
        GL.BindTexture( TextureTarget.Texture2D, texhdl )
        (*Uploads data to the texture specified for the active texture unit*)
        GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba
                            , bm.Width, bm.Height, 0, PixelFormat.Bgra
                            , PixelType.UnsignedByte, bytes)

        Some( PathTexture( texhdl, bm.Width, bm.Height, path ) )
    with
        | _ -> None
     
    


type RenderContext(inModel:ModelData, inDest:Image) =
    let model = inModel
    let dest = inDest
    let mutable sourcepath = ""
    let mutable destpath = ""
    let mutable bgimagevert = int32(0)
    let mutable bgimageidx = int32(0)
    let mutable bgshader : Shader option = None
    let mutable bgtex : PathTexture option = None
    

    let checkGLError() =
        let error = GL.GetError()
        let hasError = error = ErrorCode.NoError
        System.Diagnostics.Debug.Assert( hasError )

    let checkSourceImage() =
        let existing = if bgtex.IsSome then bgtex.Value.path else ""
        let existingmatches = existing = model.source
        if not existingmatches then
            bgtex <- createGlTexFromPath( model.source )
            checkGLError()
            
        
    let renderSourceImage() = 
        checkSourceImage()
        if bgtex.IsSome then
            GL.UseProgram( bgshader.Value.progHandle )
            GL.BindBuffer( BufferTarget.ArrayBuffer, bgimagevert )
            GL.EnableVertexAttribArray( 0 )
            GL.VertexAttribPointer( 0, 2, VertexAttribPointerType.Float, true, 8, 0)
            GL.BindBuffer( BufferTarget.ElementArrayBuffer, bgimageidx )
            (*Note we bind to texture unit 0.  This explains the sampler value below*)
            GL.ActiveTexture( TextureUnit.Texture0 )
            GL.BindTexture( TextureTarget.Texture2D, bgtex.Value.handle)
            checkGLError()
            (*Texture properties match to the texture unit, not to the texture itself*)
            GL.TexParameter( TextureTarget.Texture2D
                                , TextureParameterName.TextureMinFilter
                                , int(TextureMinFilter.Linear) )
            GL.TexParameter( TextureTarget.Texture2D
                                , TextureParameterName.TextureMagFilter
                                , int(TextureMagFilter.Linear) )
            GL.TexParameter( TextureTarget.Texture2D
                                , TextureParameterName.TextureWrapS
                                , int(TextureWrapMode.ClampToBorder) )
            GL.TexParameter( TextureTarget.Texture2D
                                , TextureParameterName.TextureWrapT
                                , int(TextureWrapMode.ClampToBorder) )
            checkGLError()
            let imageLoc = GL.GetUniformLocation( bgshader.Value.progHandle, "image")
            GL.Uniform1( imageLoc, int(TextureUnit.Texture0 - TextureUnit.Texture0) )
            checkGLError()
            GL.Disable( EnableCap.CullFace )
            GL.Disable( EnableCap.DepthTest )
            GL.Disable( EnableCap.StencilTest )
            GL.Enable( EnableCap.Blend )
            GL.BlendFuncSeparate( BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha, BlendingFactorSrc.One, BlendingFactorDest.OneMinusSrcAlpha )
            GL.DrawElements( BeginMode.Triangles, 6, DrawElementsType.UnsignedInt, 0 )
            GL.BindBuffer( BufferTarget.ArrayBuffer, 0 )
            GL.BindBuffer( BufferTarget.ElementArrayBuffer, 0 )
            GL.DisableVertexAttribArray( 0 )
            checkGLError()
            
            

    member this.checkBuffers() = 
        if bgimagevert = 0 then
            bgimagevert <- createAndUploadDataf32([|-1.f;-1.f;
                                      -1.f; 1.f; 
                                       1.f; 1.f; 
                                       1.f; -1.f |]
                                       , BufferTarget.ArrayBuffer
                                       , BufferUsageHint.StaticDraw )
            bgimageidx <- createAndUploadDatai32( [|0;1;2;2;3;0|]
                                       , BufferTarget.ArrayBuffer
                                       , BufferUsageHint.StaticDraw )

            bgshader <- compileShaderProgram( bgimage_vert, bgimage_frag )

        


    member this.render(width, height) =
        if (width > 0 && height > 0) then 
            GL.Viewport( 0, 0, width, height)
            GL.ClearColor( 1.0f, 1.0f, 1.0f, 1.0f )
            GL.Clear( ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit )
            checkGLError()
            this.checkBuffers()
            renderSourceImage()


        
        


[<EntryPoint>]
let main argv = 
    Application.Init()
    let gxml = new Glade.XML( null, "gui.glade", "mainwindow", null )
    let gobj = new GladeObj()
    gxml.Autoconnect( gobj )
    let renderer = new GLWidget(OpenTK.Graphics.GraphicsMode.Default, 3, 0, OpenTK.Graphics.GraphicsContextFlags.Default )
    let results = new Image()
    gobj.renderview.Add( renderer )
    let settings = ModelData.load()

    let rc = RenderContext( settings, results )
    let renderFrame evArgs =
        let alloc = renderer.Allocation 
        let width = alloc.Width
        let height = alloc.Height
        rc.render(width, height)

    
    let toolkit = Toolkit.Init()
    renderer.RenderFrame.Add( renderFrame )
    gobj.renderview.ShowAll();
    gobj.resultview.Add( results )
    gobj.resultview.ShowAll();
    settings.toUI(gobj)
    bindModelToUI( gobj, settings, renderer)
    

    gobj.mainwindow.Destroyed.Add( fun evArgs -> settings.fromUI(gobj); settings.save(); Application.Quit() )
    Application.Run()
    settings.save()
    0 // return an integer exit code
