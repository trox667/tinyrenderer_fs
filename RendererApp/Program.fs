// Learn more about F# at http://fsharp.org
// https://github.com/ssloy/tinyrenderer/wiki/Lesson-0:-getting-started
// https://numerics.mathdotnet.com/Matrix.html
// https://devblogs.microsoft.com/dotnet/net-core-image-processing/
// UdSSR - User-defined-Server-Side-Renderer
module RendererApp

open ImageMagick
open MathNet.Numerics.LinearAlgebra
open Math
open Renderer
open Model
open Buffer

let lessonZero() =
    let red = MagickColor(255uy, 0uy, 0uy, 255uy)
    let black = MagickColor(0uy, 0uy, 0uy, 255uy)
    let image = new MagickImage(black, 100, 100)
    image.GetPixels().GetPixel(52, 41).Set([| red.R; red.G; red.B |])
    image.Flip()
    image.Write("output.png")
    ()

let lessonOne (model : Model) =
    let roundInt = floor >> int
    let width = 500
    let height = 500
    let red = MagickColor(255uy, 0uy, 0uy, 255uy)
    let black = MagickColor(0uy, 0uy, 0uy, 255uy)
    let white = MagickColor(255uy, 255uy, 255uy, 255uy)
    let image = new MagickImage(black, width, height)
    // line3 13. 20. 80. 40. (image.GetPixels()) white
    // line3 20. 13. 40. 80. (image.GetPixels()) red
    // line3 80. 40. 13. 20. (image.GetPixels()) red
    let mapCoord (v : double) (length : int) = (v + 1.) * float length / 2.
    let pixels = image.GetPixels()
    model.faces
    |> List.iter (fun face ->
           for i in 0..2 do
               let v0 = model.vertices.[face.[i].iV]
               let v1 = model.vertices.[face.[(i + 1) % 3].iV]
               let x0 = mapCoord v0.[0] width |> roundInt
               let y0 = mapCoord v0.[1] height |> roundInt
               let x1 = mapCoord v1.[0] width |> roundInt
               let y1 = mapCoord v1.[1] height |> roundInt
               if x0 = x1 && y0 = y1 then ()
               else line3 x0 y0 x1 y1 pixels white
           ())
    image.Flip()
    image.Write("output.png")
    ()

let lessonTwo (model : Model) =
    let width = 500
    let height = 500
    let red = MagickColor(255uy, 0uy, 0uy, 255uy)
    let black = MagickColor(0uy, 0uy, 0uy, 255uy)
    let image = new MagickImage(black, width, height)
    let pixels = image.GetPixels()

    let lightDir = ( 0., 0., -1.)
    let zBuffer = createBuffer width height

    let makeScreenCoords (vertices : List<Vector<double>>) (face : FaceIndex [])
        width height =
        let v0 = vertices.[face.[0].iV]
        let v1 = vertices.[face.[1].iV]
        let v2 = vertices.[face.[2].iV]
        let convertVertexToScreen (v : Vector<double>) width height =
            ((v.[0] + 1.) * (double width) / 2.,
             (v.[1] + 1.) * (double height) / 2., 0.)
        [ convertVertexToScreen v0 width height
          convertVertexToScreen v1 width height
          convertVertexToScreen v2 width height ]
    
    let makeWorldCoords (vertices: List<Vector<double>>) (face : FaceIndex[]) =
        let v0 = vertices.[face.[0].iV]
        let v1 = vertices.[face.[1].iV]
        let v2 = vertices.[face.[2].iV]
        [
            (v0.[0], v0.[1], v0.[2]);
            (v1.[0], v1.[1], v1.[2]);
            (v2.[0], v2.[1], v2.[2])
        ]

    model.faces
    |> List.iter (fun face ->

           let screenCoords = makeScreenCoords model.vertices face width height 
           let worldCoords = makeWorldCoords model.vertices face
           
           let n =
               cross3 (sub3 worldCoords.[2] worldCoords.[0])
                   (sub3 worldCoords.[1] worldCoords.[0])
           let n = normalize3 n
           let intensity = dot3 lightDir n
           let makeComponent r = r * 255. |> byte
           let c =
               MagickColor
                   ((intensity |> makeComponent), (intensity |> makeComponent),
                    (intensity |> makeComponent), 255uy)
           if intensity > 0. then triangle screenCoords width height zBuffer pixels c
           else ())
    image.Flip()
    image.Write("output.png")
    ()

[<EntryPoint>]
let main argv =
    let model = parseModel "african_head.obj"
    // let image = new MagickImage(MagickColor("#ff0000"), 300, 200)
    // let pixels = image.GetPixels()
    // let pixel = pixels.GetPixel(0,0)
    // // pixel.SetChannel(0, uint16(0))
    // pixel.SetChannel(2, byte(255))
    // image.Write("test.png")
    // printfn "%A" (cross3 ([ 1.; 2.; 3. ] |> vector) ([ 1.; 5.; 7. ] |> vector))
    // lessonZero()
    // lessonOne model
    lessonTwo model
    0 // return an integer exit code
