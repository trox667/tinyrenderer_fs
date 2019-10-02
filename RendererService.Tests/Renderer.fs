module RendererTests

open Renderer
open MathNet.Numerics.LinearAlgebra
open System
open Xunit

[<Fact>]
let bboxMin () =
  let startVal = vector [-1.; -1.]

  let input = [
    vector [0.; 0.]
    vector [1.; 2.]
    vector [2.; 1.]
  ]

  let expect =  vector [0.; 0.]

  let v = makeBBoxMin startVal input
  Assert.StrictEqual(v, expect)

[<Fact>]
let bboxMax () =
  let startVal = vector [-1.; -1.]
  let clamp = vector [2.; 2.]
  let input = [
    vector [0.; 0.]
    vector [1.; 2.]
    vector [2.; 1.]
  ]
  let expect =  vector [2.; 2.]

  let v = makeBBoxMax startVal clamp input
  Assert.StrictEqual(v, expect)