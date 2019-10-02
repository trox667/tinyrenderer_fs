module Math

open MathNet.Numerics.LinearAlgebra

type Vec2 = double * double

type Vec3 = double * double * double

let normalize3 (v : Vec3) =
    let (x, y, z) = v
    let length = sqrt (x * x + y * y + z * z)
    (x / length, y / length, z / length)

let dot3 (v1 : Vec3) (v2 : Vec3) =
    let (v1x, v1y, v1z) = v1
    let (v2x, v2y, v2z) = v2
    v1x * v2x + v1y * v2y + v1z * v2z

let sub3 (v1 : Vec3) (v2 : Vec3) =
    let (v1x, v1y, v1z) = v1
    let (v2x, v2y, v2z) = v2
    (v1x - v2x, v1y - v2y, v1z - v2z)

let multiplyVectors (v1 : Vec3) (v2 : Vec3) =
    let (v1x, v1y, v1z) = v1
    let (v2x, v2y, v2z) = v2
    (v1x * v2x, v1y * v2y, v1z * v2z)

let multiplyVectorLists (v1 : Vec3) (v2 : Vec3) = multiplyVectors v1 v2

let cross3 (v1 : Vec3) (v2 : Vec3) =
    let (v1x, v1y, v1z) = v1
    let (v2x, v2y, v2z) = v2
    sub3 (multiplyVectorLists (v1y, v1z, v1x) (v2z, v2x, v2y))
        (multiplyVectorLists (v1z, v1x, v1y) (v2y, v2z, v2x))
