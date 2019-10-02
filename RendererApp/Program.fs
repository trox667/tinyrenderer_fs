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
    let width = 200
    let height = 200
    let red = MagickColor(255uy, 0uy, 0uy, 255uy)
    let black = MagickColor(0uy, 0uy, 0uy, 255uy)
    let image = new MagickImage(black, width, height)
    let pixels = image.GetPixels()
    let rand = System.Random()
    let lightDir = vector [ 0.; 0.; -1. ]
    // triangle [ (vector [ 10.; 10.; 0. ])
    //            (vector [ 100.; 30.; 0. ])
    //            (vector [ 190.; 160.; 0. ]) ] width height (image.GetPixels()) red
    model.faces
    |> List.iter (fun face ->
           let mutable screenCoords =
               [ (vector [ 0.; 0.; 0. ])
                 (vector [ 0.; 0.; 0. ])
                 (vector [ 0.; 0.; 0. ]) ]

           let mutable worldCoords =
               [ (vector [ 0.; 0.; 0. ])
                 (vector [ 0.; 0.; 0. ])
                 (vector [ 0.; 0.; 0. ]) ]

           for i in 0..2 do
               let v = model.vertices.[face.[i].iV]
               screenCoords.[i].SetValues [| (v.[0] + 1.) * (double width)
                                             / 2.
                                             (v.[1] + 1.) * (double height)
                                             / 2.
                                             0. |]
               worldCoords.[i].SetValues [| v.[0]
                                            v.[1]
                                            v.[2] |]
           let n =
               cross3 (worldCoords.[2] - worldCoords.[0])
                   (worldCoords.[1] - worldCoords.[0])
           let n = n.Normalize 1.
           let intensity = lightDir.DotProduct n
           let makeComponent r = r * 255. |> byte
           let c =
               MagickColor
                   ((intensity |> makeComponent), (intensity |> makeComponent),
                    (intensity |> makeComponent), 255uy)
           if intensity > 0. then triangle screenCoords width height pixels c
           else ())
    image.Flip()
    image.Write("output.png")
    ()

[<EntryPoint>]
let main argv =
    let model = parseModel "../african_head.obj"
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
