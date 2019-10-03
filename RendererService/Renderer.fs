module Renderer

open Math
open ImageMagick
open MathNet.Numerics.LinearAlgebra

open Buffer

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

let barycentric (p0 : Vec3) (p1 : Vec3) (p2 : Vec3) (P : Vec3) =
    let (p0x, p0y, _) = p0
    let (p1x, p1y, _) = p1
    let (p2x, p2y, _) = p2
    let (pX, pY, _) = P

    let u = cross3 (p2x - p0x, p1x - p0x, p0x - pX) (p2y - p0y, p1y - p0y, p0y - pY)
    let (ux, uy, uz) = u;

    if abs (uz) < 1. then (-1., 1., 1.)
    else
        (1. - (ux + uy) / uz, uy / uz, ux / uz)

let makeBBoxMin (startVal : Vec3) (pts : List<Vec3>) =
    List.fold (fun (acc : Vec3) (curr : Vec3) ->
        let (ax, ay, _) = acc
        let (cx, cy, _) = curr
        (min ax cx |> max 0., min ay cy |> max 0., 0.)) startVal pts

let makeBBoxMax (startVal : Vec3) (clamp : Vec3) (pts : List<Vec3>) =
    List.fold (fun (acc : Vec3) (curr : Vec3) ->
        let (ax, ay, _) = acc
        let (cx, cy, _) = curr
        let (clx, cly, _) = clamp
        (max ax cx |> min clx, max ay cy |> min cly, 0.)) startVal pts

let makeZ (pts : List<Vec3>) (bcScreen: Vec3) =
    let (x, y, z) = bcScreen
    let bcList = [x; y; z]
    List.fold2 (fun (acc:double) (pt:Vec3) (v:double) -> 
        let (_,_,y) = pt
        acc + y * v)
         0. pts bcList
    

let triangle (pts : List<Vec3>) (width : int) (height : int) (zBuffer: double[])
    (pixels : IPixelCollection) (color : MagickColor) =
    let clamp = (double width - 1., double height - 1., 1.)
    let bboxmin = makeBBoxMin (System.Double.MaxValue, System.Double.MaxValue, System.Double.MaxValue) pts
    let bboxmax = makeBBoxMax (-System.Double.MaxValue, -System.Double.MaxValue, -System.Double.MaxValue) clamp pts

    let (xMin, yMin, _) = bboxmin
    let (xMax, yMax, _) = bboxmax

    let xMin = xMin |> roundInt
    let xMax = xMax |> roundInt
    let yMin = yMin |> roundInt
    let yMax = yMax |> roundInt

    for x = xMin to xMax do
        for y = yMin to yMax do
            let (bcsx, bcsy, bcsz) =
                barycentric pts.[0] pts.[1] pts.[2] (double x, double y, 0.)
            if bcsx < 0. || bcsy < 0. || bcsz < 0. then
                ()
            else 
                let z = makeZ pts (bcsx, bcsy, bcsz)
                if getBufferValue zBuffer width x y < z then
                    updateBuffer zBuffer width x y z
                    pixels.GetPixel(x, y).Set([| color.R; color.G; color.B |])
                    
    ()
