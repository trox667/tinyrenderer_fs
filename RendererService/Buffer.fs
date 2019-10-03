module Buffer

let createBuffer width height = 
    Array.create (width * height) System.Double.MinValue

let updateBuffer (buffer: double[]) width x y value =
  let i = x + y * width
  Array.fill buffer i 1 value

let getBufferValue (buffer: double[]) width x y =
  let i = x + y * width
  buffer.[i]