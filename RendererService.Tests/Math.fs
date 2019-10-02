module MathTests

open Math

open MathNet.Numerics.LinearAlgebra
open System
open Xunit

[<Fact>]
let crossProduct () =
    let expected = vector ([-1.; -4.; 3.])
    let actual = cross3 ([ 1.; 2.; 3. ] |> vector) ([ 1.; 5.; 7. ] |> vector)
    Assert.StrictEqual(expected, actual)