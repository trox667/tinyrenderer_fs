module MathTests

open Math

open MathNet.Numerics.LinearAlgebra
open System
open Xunit

[<Fact>]
let crossProductTest () =
    let expected = (-1., -4., 3.)
    let actual = cross3 (1., 2., 3.) (1., 5., 7.)
    Assert.StrictEqual(expected, actual)

[<Fact>]
let dotProductTest () =
  let expected = 122.
  let actual = dot3 (9., 2., 7.) (4., 8., 10.)
  Assert.StrictEqual(expected, actual)

[<Fact>]
let normalizeTest () =
  let expected = (1., 0., 0.)
  let actual = normalize3 (5., 0., 0.)
  Assert.StrictEqual(expected, actual)

[<Fact>]
let subTest () =
  let expected = (0., 0., 0.)
  let actual = sub3 (3., 2., 1.) (3., 2., 1.)
  Assert.StrictEqual(expected, actual)