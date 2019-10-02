module Renderer

open Math
open ImageMagick
open MathNet.Numerics.LinearAlgebra

let roundInt = floor >> int

let line (x0 : float) (y0 : float) (x1 : float) (y1 : float)
    (pixels : IPixelCollection) (color : MagickColor) =
    for t = 0 to 100 do
        let x = x0 + (x1 - x0) * (float t) / 100.
        let y = y0 + (y1 - y0) * (float t) / 100.
        pixels.GetPixel(roundInt x, roundInt y)
              .Set([| color.R; color.G; color.B |])
    ()

let line2 (x0 : float) (y0 : float) (x1 : float) (y1 : float)
    (pixels : IPixelCollection) (color : MagickColor) =
    for x = roundInt x0 to roundInt x1 do
        let t = (float x - x0) / (x1 - x0)
        let y = y0 * (1. - t) + y1 * t |> roundInt
        pixels.GetPixel(x, y).Set([| color.R; color.G; color.B |])
    ()

let line3 (x0 : int) (y0 : int) (x1 : int) (y1 : int)
    (pixels : IPixelCollection) (color : MagickColor) =
    let (steep, x0, y0, x1, y1) =
        match abs (x0 - x1) < abs (y0 - y1) with
        | true -> (true, y0, x0, y1, x1)
        | _ -> (false, x0, y0, x1, y1)

    let (x0, y0, x1, y1) =
        match x0 > x1 with
        | true -> (x1, y1, x0, y0)
        | _ -> (x0, y0, x1, y1)

    let clamp v min max =
        match (v, min, max) with
        | (v, min, _) when v < min -> min
        | (v, _, max) when v > max -> max
        | (v, _, _) -> v

    for x = x0 to x1 do
        let t = (float x - float x0) / (float x1 - float x0)
        let y = float y0 * (1. - t) + float y1 * t |> roundInt
        let x = clamp x 0 499
        let y = clamp y 0 499
        match steep with
        | true -> pixels.GetPixel(y, x).Set([| color.R; color.G; color.B |])
        | _ -> pixels.GetPixel(x, y).Set([| color.R; color.G; color.B |])
    ()

let barycentric (p0 : Vector<double>) (p1 : Vector<double>)
    (p2 : Vector<double>) (P : Vector<double>) =
    let u =
        cross3 (vector [ p2.[0] - p0.[0]
                         p1.[0] - p0.[0]
                         p0.[0] - P.[0] ]) (vector [ p2.[1] - p0.[1]
                                                     p1.[1] - p0.[1]
                                                     p0.[1] - P.[1] ])
    if abs (u.[2]) < 1. then vector [ -1.; 1.; 1. ]
    else
        vector [ 1. - (u.[0] + u.[1]) / u.[2]
                 u.[1] / u.[2]
                 u.[0] / u.[2] ]

let makeBBoxMin (startVal:Vector<double>) (pts : List<Vector<double>>) =
    List.fold (fun (acc : Vector<double>) (curr : Vector<double>) ->
        vector [ min acc.[0] curr.[0] |> max 0.
                 min acc.[1] curr.[1] |> max 0. ]) startVal pts

let makeBBoxMax (startVal:Vector<double>) (clamp: Vector<double>) (pts : List<Vector<double>>) =
    List.fold (fun (acc : Vector<double>) (curr : Vector<double>) ->
        vector [ max acc.[0] curr.[0] |> min clamp.[0]
                 max acc.[1] curr.[1] |> min clamp.[0] ]) startVal pts

let triangle (pts : List<Vector<double>>) (width : int) (height : int)
    (pixels : IPixelCollection) (color : MagickColor) =
    
    let clamp =
        vector [ double width - 1.
                 double height - 1. ]
    let bboxmin = makeBBoxMin (vector [-1.; -1.]) pts
    let bboxmax = makeBBoxMax (vector [0.; 0.]) clamp pts
    
    let xMin = bboxmin.[0] |> roundInt
    let xMax = bboxmax.[0] |> roundInt
    let yMin = bboxmin.[1] |> roundInt
    let yMax = bboxmax.[1] |> roundInt
    for x = xMin to xMax do
        for y = yMin to yMax do
            let bcScreen =
                barycentric pts.[0] pts.[1] pts.[2] (vector [ double x
                                                              double y
                                                              0. ])
            if bcScreen.[0] < 0. || bcScreen.[1] < 0. || bcScreen.[2] < 0. then
                ()
            else pixels.GetPixel(x, y).Set([| color.R; color.G; color.B |])
    ()
