module RendererTests

open Renderer
open MathNet.Numerics.LinearAlgebra
open System
open Xunit

[<Fact>]
let bboxMin () =
  let startVal = (-1., -1., -1.)

  let input = [
    (0., 0., 0.)
    (1., 2., 0.)
    (2., 1., 0.)
  ]

  let expect =  (0., 0., 0.)

  let v = makeBBoxMin startVal input
  Assert.StrictEqual(v, expect)

[<Fact>]
let bboxMax () =
  let startVal = (-1., -1., -1.)
  let clamp = (2., 2., -1.)
  let input = [
    (0., 0., 0.)
    (1., 2., 0.)
    (2., 1., 0.)
  ]
  let expect =  (2., 2., 0.)

  let v = makeBBoxMax startVal clamp input
  Assert.StrictEqual(v, expect)