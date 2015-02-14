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


let pheight =  0.40824829042f
let radius = 0.57725026913f
let angles = [|0.0f; 2.0f*float32(Math.PI)/3.0f; 4.0f*float32(Math.PI)/3.0f|]
let sincos (x:float32) = (radius*float32(Math.Cos(float(x))), radius*float32(Math.Sin(float(x))))
let multtable = Array.map sincos angles
let tridata = [| 0.0f; 0.0f; 0.40824829042f;
                 fst(multtable.[0]); snd(multtable.[0]); 0.0f;
                 fst(multtable.[1]); snd(multtable.[1]); 0.0f;
                 fst(multtable.[2]); snd(multtable.[2]); 0.0f; |]

//These are the UV points of the triangles formed by the dest image unfolded
let triuv = [|Vector2(0.5f, 0.5f); //center point
                Vector2(0.0f, 0.0f); //points clockwise from top left
                Vector2(1.0f, 0.0f);
                Vector2(1.0f, 1.0f);
                Vector2(0.0f, 1.0f);|]

//Any triangle with these two indicies is a bad triangle; we won't project it.
//On the other hand, both of these indexes need to fold into the first index
//as that is what happens when folded.
let folded = (3,4)

let toBarycentric( a:Vector2, b:Vector2, c:Vector2, p:Vector2 ) = 
    let v0 = b - a
    let v1 = c - a
    let v2 = p - a
    let d00 = Vector2.Dot(v0, v0);
    let d01 = Vector2.Dot(v0, v1);
    let d11 = Vector2.Dot(v1, v1);
    let d20 = Vector2.Dot(v2, v0);
    let d21 = Vector2.Dot(v2, v1);
    let invDenom = 1.0f / (d00 * d11 - d01 * d01);
    let v = (d11 * d20 - d01 * d21) * invDenom;
    let w = (d00 * d21 - d01 * d20) * invDenom;
    let u = 1.0f - v - w;
    Vector3(u,v,w)

let uvToIndexedBarycentric( u, v ) =
    let input = Vector2(u,v)
    let vecdistance( point) = 
        let distance = point - input
        distance.Length

    let unpackDistTuple tuple = snd(tuple)
    let distances = Array.mapi (fun i x -> (i, vecdistance(x))) triuv
    let mutable sorted = Array.sortBy (fun item -> snd(item)) distances
    //Take the top 3 items
    Array.Resize( &sorted, 3 )
    let isInFolded( item ) = 
        let data = fst(item)
        data = fst(folded) || data = snd(folded)
    let numFolded = Seq.length( (Seq.filter isInFolded sorted) )
    if numFolded < 2 then
        let doFold(item) =
            let data = fst(item)
            let remapped = if ( data ) = snd(folded) then fst(folded)
                            else data
            (remapped, snd(item))

        let foldedSorted = Array.map doFold sorted
        let barycentric = toBarycentric( triuv.[fst(sorted.[0])]
                                        , triuv.[fst(sorted.[1])] 
                                        , triuv.[fst(sorted.[2])]
                                        , input )

        let retval = [|(fst(foldedSorted.[0]), barycentric.X);
                        (fst(foldedSorted.[1]), barycentric.Y);
                        (fst(foldedSorted.[2]), barycentric.Z)|]
        Some( retval )
    else
        None

let weightedVec3( a:Vector3, b:float32 ) = Vector3( a.X*b, a.Y*b, a.Z*b )

let sumVec3(a:Vector3, b:Vector3) = Vector3( a.X+b.X, a.Y+b.Y, a.Z+b.Z)

let triDataToVec3(a) =
    let localIdx = a*3
    Vector3(tridata.[localIdx], tridata.[localIdx+1], tridata.[localIdx+2])

let doWeight (data : (int*float32)) =
    let vec = triDataToVec3( fst(data ))
    weightedVec3( vec, snd(data) )

let r3pointFromBarycentric ( data : (int * float32) [] ) =
    let v0 = doWeight(data.[0])
    let v1 = doWeight(data.[1])
    let v2 = doWeight(data.[2])
    sumVec3( sumVec3( v0, v1 ), v2 )
    
let projectR3toR2( point:Vector3, mat:Matrix4 ) = 
    let fullpoint = Vector4(point, 1.0f)
    let transformed = Vector4.Transform( fullpoint, mat )
    Vector2(transformed.X / transformed.W, transformed.Y / transformed.W)


let clamp( data:float32, min:float32, max:float32) = Math.Min( Math.Max( data, min), max )
   

let projectR2ToUV( point:Vector2 ) =
    //data is projected into a space that is -1, 1.  We need to move that space
    //to 0, 1
    let intermediate = point + Vector2(1.0f, 1.0f)
    Vector2( clamp( intermediate.X / 2.0f, 0.0f, 1.0f ), clamp( intermediate.Y / 2.0f , 0.0f, 1.0f ))

type PixelData =
    struct
        val R:byte
        val G:byte
        val B:byte
        val A:byte
        new( r, g, b, a ) = { R=r; G=g; B=b; A=a}
    end

type ByteImage(_data:byte array, _width:int, _height:int) =
    let mutable data = _data
    let width = _width
    let height = _height
    let stride = _width * 4
    
    let offsetFromUV( pos:Vector2 ) = 
        let x = int(float32(width-1) * pos.X + 0.5f);
        let y = int(float32(height-1) * pos.Y + 0.5f);
        let offset = y*stride + x*4;
        offset

    member this.getWidth() = width
    member this.getHeight() = height
    member this.getStride() = stride;
    
    //Assuming RGBA image data.  Assumed clamped UV.
    member this.ReadPixel( pos ) =
        let offset = offsetFromUV( pos )
        PixelData( data.[offset], data.[offset+1], data.[offset+2], data.[offset+3])
    
    member this.WritePixel( pos, pixel:PixelData ) = 
        let offset = offsetFromUV( pos )
        data.[offset] <- pixel.R
        data.[offset+1] <- pixel.G
        data.[offset+2] <- pixel.B
        data.[offset+3] <- pixel.A


let pyramidProject( src:ByteImage, dest:ByteImage, mvp ) =
    for h in 0..(dest.getHeight() - 1) do
        for w in 0..(dest.getWidth() - 1) do
           let u = float32(w) / float32(dest.getWidth())
           let v = float32(h) / float32(dest.getHeight())
           let barycentric = uvToIndexedBarycentric(u,v)
           let pixel = 
                if ( barycentric.IsSome ) then
                    let r3point = r3pointFromBarycentric( barycentric.Value )
                    let r2point = projectR3toR2(r3point, mvp)
                    let uvpoint = projectR2ToUV( r2point )
                    src.ReadPixel( uvpoint )
                else
                    PixelData(byte(255), byte(255), byte(255), byte(0))
           dest.WritePixel(Vector2(u,v), pixel)





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

let pyramid_vert =
    "#version 130\n\
    in vec3 vpos;\n\
    uniform mat4 mvp;\n\
    uniform mat4 mv;\n\
    out vec3 worldpos;\n\
    void main()\n\
    {\n\
    \tgl_Position = mvp * vec4(vpos, 1.0);\n\
    \tworldpos = (mv * vec4(vpos, 1.0)).xyz;\n\
    }\n"

    (*Use a normal derived from the world position in order to highlight faces*)
let pyramid_frag =
    "#version 130\n\
    out vec4 fragData;\n\
    in vec3 worldpos;\n\
    void main()\n\
    {\n\
    \tvec3 X = dFdx(worldpos);\n\
    \tvec3 Y = dFdy(worldpos);\n\
    \tvec3 normal = normalize(cross(X,Y));\n\
    \tfloat colorMult = abs(dot(normal, normalize(vec3(0,1,-1))));\n\
    \tfragData = vec4( 1.0 * colorMult, 0.0, 0.0, .5 );\n\
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

    member this.setUniform( name, value:int ) =
        let uniLoc = this.getUniLoc( name )
        if uniLoc.IsSome then
            GL.Uniform1( uniLoc.Value, value )

    member this.setUniform( name, value:Matrix4 byref ) =
        let uniLoc = this.getUniLoc( name )
        if uniLoc.IsSome then
            GL.UniformMatrix4( uniLoc.Value, false, &value )
        

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

let loadBitmapBytes( path ) = 
    let stream = new FileStream( path, FileMode.Open )
    let bm = new System.Drawing.Bitmap( stream )
    let bmdata = bm.LockBits( System.Drawing.Rectangle(0, 0, bm.Width, bm.Height)
                                , System.Drawing.Imaging.ImageLockMode.ReadOnly
                                , System.Drawing.Imaging.PixelFormat.Format32bppArgb)

    let length = bmdata.Stride * bm.Height
    let bytes : byte array = Array.zeroCreate length
    Marshal.Copy( bmdata.Scan0, bytes, 0, length)
    bm.UnlockBits( bmdata )
    stream.Close()
    (bytes, bmdata)
 
let createGlTexFromPath( path ) = 
    try
        let(bytes,bmdata) = loadBitmapBytes path
        (*flip the rows so the image is right-side up*)
        let row : byte array = Array.zeroCreate bmdata.Stride
        for h in 0..(bmdata.Height/2) do
            let endRow = bmdata.Height - h - 1
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
                            , bmdata.Width, bmdata.Height, 0, PixelFormat.Bgra
                            , PixelType.UnsignedByte, bytes)

        Some( PathTexture( texhdl, bmdata.Width, bmdata.Height, path ) )
    with
        | _ -> None
     

let buildMVPFromModel( rotation, scale, translation:Vector2, aspect:float32 ) =
    let rotation = cos(rotation),sin(rotation)
    let mutable rotMat = Matrix4.Identity;
    rotMat.Row0 <- Vector4(fst(rotation), -1.0f * snd(rotation), 0.0f, 0.0f)
    rotMat.Row1 <- Vector4(snd(rotation), fst(rotation), 0.0f, 0.0f)
    let mutable scaleMat = Matrix4.Identity
    scaleMat.Row0 <- Vector4( scale, 0.0f, 0.0f, 0.0f )
    scaleMat.Row1 <- Vector4( 0.0f, scale, 0.0f, 0.0f )
    scaleMat.Row2 <- Vector4( 0.0f, 0.0f, scale, 0.0f )
    let mutable translationMat = Matrix4.Identity
    translationMat.Row3 <- Vector4( translation.X, translation.Y, 0.0f, 1.0f )

    let mutable projection = Matrix4.Identity
    projection.Row1 <- Vector4( 0.0f, aspect, 0.0f, 0.0f )

    let model = Matrix4.Mult( Matrix4.Mult( rotMat, scaleMat ), translationMat )
    let mvp = Matrix4.Mult(projection, model )
    mvp


type RenderContext(inModel:ModelData, inDest:Image) =
    let model = inModel
    let dest = inDest
    let mutable sourcepath = ""
    let mutable destpath = ""
    let mutable bgimagevert = int32(0)
    let mutable bgimageidx = int32(0)
    let mutable bgshader : Shader option = None
    let mutable bgtex : PathTexture option = None
    let mutable pyramidvert = int32(0)
    let mutable pyramididx = int32(0)
    let mutable pyramidshder : Shader option = None
    

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
       
       

    let checkImageBuffers() = 
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
            

    let checkPyramidData() = 
        if pyramidvert = 0 then
            let multtable = Array.map sincos angles
            pyramidvert <- createAndUploadDataf32( 
                 tridata
                 , BufferTarget.ArrayBuffer
                 , BufferUsageHint.StaticDraw )
            pyramididx <- createAndUploadDatai32(
                            [|0; 1; 2;
                              0; 2; 3;
                              0; 3; 1;|]
                               , BufferTarget.ElementArrayBuffer
                               , BufferUsageHint.StaticDraw )
            pyramidshder <- compileShaderProgram( pyramid_vert, pyramid_frag )
              
        
    let renderSourceImage() = 
        checkSourceImage()
        if bgtex.IsSome then
            checkImageBuffers()
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
            bgshader.Value.setUniform( "image", 0)
            checkGLError()
            GL.Disable( EnableCap.CullFace )
            GL.Disable( EnableCap.DepthTest )
            GL.Disable( EnableCap.StencilTest )
            GL.Enable( EnableCap.Blend )
            GL.BlendFuncSeparate( BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha
                                    , BlendingFactorSrc.One, BlendingFactorDest.OneMinusSrcAlpha )
            GL.DrawElements( BeginMode.Triangles, 6, DrawElementsType.UnsignedInt, 0 )
            GL.BindBuffer( BufferTarget.ArrayBuffer, 0 )
            GL.BindBuffer( BufferTarget.ElementArrayBuffer, 0 )
            GL.DisableVertexAttribArray( 0 )
            checkGLError()
        
    let renderPyramid( aspect ) =
        checkPyramidData()
        if pyramidshder.IsSome then
            GL.UseProgram( pyramidshder.Value.progHandle )
            
            GL.BindBuffer( BufferTarget.ArrayBuffer, pyramidvert )
            GL.EnableVertexAttribArray( 0 )
            GL.VertexAttribPointer( 0, 3, VertexAttribPointerType.Float, true, 12, 0)
            GL.BindBuffer( BufferTarget.ElementArrayBuffer, pyramididx )
            (*build mvp*)
            let mutable mvp = buildMVPFromModel( model.rotation, model.scale, model.translation, aspect )
            pyramidshder.Value.setUniform( "mvp", &mvp )
            pyramidshder.Value.setUniform( "mv", &mvp )
            GL.Disable( EnableCap.CullFace )
            GL.Enable( EnableCap.DepthTest )
            GL.Disable( EnableCap.StencilTest )
            GL.Enable( EnableCap.Blend )
            GL.BlendFuncSeparate( BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha
                                , BlendingFactorSrc.One, BlendingFactorDest.OneMinusSrcAlpha )
            
            GL.DrawElements( BeginMode.Triangles, 9, DrawElementsType.UnsignedInt, 0 )
            GL.BindBuffer( BufferTarget.ArrayBuffer, 0 )
            GL.BindBuffer( BufferTarget.ElementArrayBuffer, 0 )
            GL.DisableVertexAttribArray( 0 )
            checkGLError()
        
            

    member this.render(width, height) =
        let mutable targetAspect = 1.0f
        if (width > 0 && height > 0) then 
            GL.Viewport( 0, 0, width, height)
            GL.ClearColor( 1.0f, 1.0f, 1.0f, 1.0f )
            GL.Clear( ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit )
            checkSourceImage()
            checkGLError()
            //setup viewport to match the aspect ratio
            if bgtex.IsSome then
                let img = bgtex.Value
                targetAspect <- float32(img.width) / float32(img.height)
                let existingAspect = float32(width)/float32(height)
                //we need to pad the top and the bottom till the aspects match
                if targetAspect > existingAspect then
                    let targetHeight = int32(float32(width) / targetAspect)
                    let diff = (height - int32(targetHeight)) / 2
                    GL.Viewport( 0, diff, width, targetHeight )
                else
                    let targetWidth = int32(targetAspect * float32(height))
                    let diff = (width - targetWidth) / 2
                    GL.Viewport( diff, 0, targetWidth, height)

            renderSourceImage()
            renderPyramid(targetAspect)

        targetAspect
 
        
let buildTargetBitmap( model:ModelData, img : Image, aspect ) = 
    try
        let mvp = buildMVPFromModel( -1.0f * model.rotation, model.scale, model.translation, aspect )
        let length = model.destsize * model.destsize * 4;
        let destbytes : byte array = Array.zeroCreate length
        let (bytes,srcbits) = loadBitmapBytes( model.source )
        pyramidProject( ByteImage( bytes, srcbits.Width, srcbits.Height )
                    , ByteImage(destbytes, model.destsize, model.destsize)
                    , mvp )
        let bmap = new System.Drawing.Bitmap(model.destsize, model.destsize)
        let bmapbits = bmap.LockBits( Drawing.Rectangle( 0, 0, model.destsize, model.destsize)
                                    , Drawing.Imaging.ImageLockMode.WriteOnly
                                    , Drawing.Imaging.PixelFormat.Format32bppArgb );
        Marshal.Copy( destbytes, 0, bmapbits.Scan0, length )
        let ms = new MemoryStream ();
        bmap.Save (ms, Drawing.Imaging.ImageFormat.Png);
        ms.Seek(int64(0), SeekOrigin.Begin) |> ignore
        let pmap = new Gdk.Pixbuf(ms)
        img.Pixbuf <- pmap;
        if model.destination.Length > 0 then
            try
                bmap.Save( model.destination )
            with
                | except -> ()
    with
        | except -> ()


    

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
        let aspect =  rc.render(width, height)
        buildTargetBitmap(settings, results, aspect)

    
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
