module Math

open MathNet.Numerics.LinearAlgebra

type Vec2 = double * double
type Vec3 = double * double * double

let multiplyVector (v1 : Vector<double>) (v2 : Vector<double>) =
    Vector.map2 (*) v1 v2
let multiplyVectorLists (v1 : List<double>) (v2 : List<double>) =
    multiplyVector (vector v1) (vector v2)

let cross3 (v1 : Vector<double>) (v2 : Vector<double>) =
    assert (v1.Count = v2.Count && v1.Count = 3)
    let (x1, y1, z1) = (v1.[0], v1.[1], v1.[2])
    let (x2, y2, z2) = (v2.[0], v2.[1], v2.[2])
    multiplyVectorLists [ y1; z1; x1 ] [ z2; x2; y2 ] - multiplyVectorLists [ z1; x1; y1 ] [ y2; z2; x2 ]